;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; abstract.lisp --
;; Author: Hassen Saidi
;; Created On      : Sat Sep 19, 1998
;; Last Modified By: Hassen Saidi
;; Last Modified On: Sat Jun 1999
;; Status          : Unknown, Use with caution!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Abstract interpretation proof strategy.
;; 
;; Given a state of record type such as 
;;      state : TYPE [#y1, y2: nat; location : bool #]
;; and a set of state predicates such as
;;      LAMBDA(s:state): y1(s)=0,  
;;      LAMBDA(s:state): y2(s)=0 and 
;;      LAMBDA(s:state): y1(s)<=y2(s)
;;  
;; an abstract state type abs_state is created
;;      abs_state : TYPE [# B1,B2,B3:bool; location : bool #]
;;
;; where the components B1..B3 encode the value of the three predicates
;; defined above and where the components of finite type in state are preserved
;; 
;; The proof command abstract translates any PVS boolean formula (proof-goal) 
;; depending on the type state to a PVS boolean formula depending on the
;; type abs_state. The resulting formula is stronger than the original one
;; but depends only on finite types and can be discharged with model-check.
;; 
;;
;; the strategy abstract-and-mc applies abstract and then model-checks
;; the resulting formula.
;;

(in-package 'pvs)

;;

(defvar *history-bdd-hash-encoding* nil)
(defvar *state-type* nil)
(defvar *abs-state-type* nil)
(defvar *list-predicates* nil)
(defvar *state-var-1* nil)
(defvar *state-var-2* nil)
(defvar *abs-state-var-1* nil)
(defvar *abs-state-var-2* nil)
(defvar *make-abs-rec-app* nil)
(defvar *use-context?* nil)
(defvar *bool-hash-table* nil)
(defvar *abstraction-strategy* nil)

;;

(defvar *pvs-abstract-bdd-hash* nil)
(defvar *abstract-bdd-pvs-hash* nil)
(defvar *abstract-bool-var-hash* nil)
(defvar *primed-variables* nil) 
(defvar *unprimed-variables* nil)
(defvar *list-indices* nil)
(defvar *list-primed-indices* nil)
(defvar *dep-list-indices* nil)
(defvar *dep-list-primed-indices* nil)
(defvar *list-not-affected* nil)
(defvar *counter-possible-prf* 0)
(defvar *counter-attempt* 0)
(defvar *proof-counter* 0)
(defvar *max-proof-counter* 0)

(defvar *static-analysis?* nil)
(defvar *exclusive?* nil)

(defvar *primed-exclusive* nil)
(defvar *unprimed-exclusive* nil)


(defvar *abstract-cession* 0)
(defvar *assoc-bindings* nil)
(defvar *new-abstract-field-ids* nil)

;;

(defstep abstract-and-mc (list-state-predicates
			  &optional (cases-rewrite? T) exclusive?
			  ;; means that B1, ...,Bk are exclusive .
					 (proof-counter 10))
 (apply (try (abstract list-state-predicates :cases-rewrite? cases-rewrite?
		       :exclusive? exclusive? :proof-counter proof-counter)
	     (musimp)
	     (skip)))
  "Proof strategy that invokes abstract the boolean abstraction 
   proof command and and then model-checks the result.
   Has the same arguments as abstract."
  "Applies boolean abstraction 
   and prove by model-checking!")

(defstep abstract (list-state-predicates
		   &optional (cases-rewrite? T) exclusive?
		   ;; means that B1, ...,Bk are exclusive .
		   (proof-counter 10)
		   )
  (let ((cuth *current-theory*)
        (cuthstr (string (id cuth)))
        )
        (then* 
               (auto-rewrite-theory cuthstr :always? T)
               (auto-rewrite-theory "ctlops" :defs T :always? T)
               (auto-rewrite "/=")
               (rewrite-msg-off)
               (assert :cases-rewrite? cases-rewrite?)
               (expand "EX")
               (assert :cases-rewrite? cases-rewrite?)
               (abs-simp list-state-predicates
			 :exclusive? exclusive? :proof-counter proof-counter)
               (simplify)))
  "Rewrites temporal operators into mu/nu expressions, and rewrites
   definitions just like model-check does. Then, Applies boolean abstraction.
   The parameter cases-rewrite? can be set to nil to avoid rewriting and 
   simplification within unresolved selections within CASES expressions 
   for the sake of efficiency.
   The parameter exclusive? indicates weather the abstraction predicates
   are exclusive. This allows a more efficient computation of the abstraction.
   The parameter proof-counter indicates the maximal number of calls to the
   decision procedure  when abstracting each atom of the sequent.  Any number 
   allows to compute a faithful abstraction. However, it is preferable 
   that the number is at least equal twice the length of the list of 
   predicates."
  "Applying boolean abstraction after Rewriting and simplifying")


(addrule 'abs-simp (list-state-predicates)
	 ((fnums *) (exclusive? nil) (proof-counter 10))
       (abstract-fun list-state-predicates fnums t exclusive? proof-counter)
	 "Abstraction computation with respect a list of predicates.
          Computation uses over and under approximation of atoms"
	 "~%Computing abstraction,")

(defun abstract-fun (list-state-predicates
		     &optional fnums use-context? exclusive? proof-counter)
  #'(lambda (ps)
      (run-abstraction ps fnums list-state-predicates use-context? exclusive?
		       proof-counter)))


(defun run-abstraction (ps fnums list-state-predicates use-context?
			   exclusive? proof-counter)
  (bdd_init)
  (check-abstract-fun-arguments ps list-state-predicates fnums
				use-context? exclusive? proof-counter)
  (let* ((*bdd-initialized* t)
         (*bound-variables* nil)
         (*assoc-bindings* nil)
         (*generate-tccs* 'NONE)
         (*abstraction-strategy* nil)
         (*max-proof-counter* proof-counter)
         (*use-context?* use-context?)
         (*exclusive?* exclusive?)
	 (*new-abstract-field-ids* nil)
         (sforms (s-forms (current-goal ps)))
	 (selected-sforms (select-seq sforms fnums))
	 (remaining-sforms (delete-seq sforms fnums))
         (formula-to-abstract 
           (make-conjunction
	     (mapcar #'(lambda (sf) (formula sf))
	       selected-sforms)))
         (abstract-formula 
	  (typecheck (compute-abstract-formula formula-to-abstract
					       list-state-predicates))))
   (multiple-value-prog1
     (create-subgoal-with-new-formula 
        ps sforms remaining-sforms abstract-formula)
     (bdd_quit))))

(defun check-abstract-fun-arguments (ps list-state-predicates
					   fnums use-context? exclusive?
					   proof-counter)
  (unless (if (consp list-state-predicates)
	      (every #'stringp list-state-predicates)
	      (stringp list-state-predicates))
    (error-format-if
     "LIST-STATE-PREDICATES should be a string or list of strings"))
  (unless (select-seq (s-forms (current-goal ps)) fnums)
    (error-format-if "~a is not a valid FNUMS reference" fnums))
  (unless (memq use-context? '(nil t))
    (error-format-if "USE-CONTEXT? must be NIL or T"))
  (unless (memq exclusive? '(nil t))
    (error-format-if "EXCLUSIVE? must be NIL or T"))
  (unless (integerp proof-counter)
    (error-format-if "PROOF-COUNTER must be an integer")))

(defun create-subgoal-with-new-formula (ps 
              sforms remaining-sforms abstract-formula)
 (let ((computed-fml abstract-formula))
   (if computed-fml (add-abstract-subgoals ps sforms
                  (list (conjuncts abstract-formula))  remaining-sforms)
        (progn 	
           (format t "~%Fail to abstract to finite state:")
           (values 'X nil nil)))))


(defun add-abstract-subgoals (ps 
              sforms conjuncts remaining-sforms)
   (let ((subgoals
	 (mapcar #'(lambda (c)
		     (create-abstract-subgoal c ps sforms remaining-sforms))
	   conjuncts)))
    (if (and (singleton? subgoals)
	     (subsetp (s-forms (car subgoals)) sforms)
	     (subsetp sforms (s-forms (car subgoals))))
	(values 'X nil nil)
	(values '? subgoals))))

(defun create-abstract-subgoal (conjunct ps sforms remaining-sforms)
    (copy (current-goal ps)
    's-forms (nconc
	      (mapcar #'(lambda (fmla)
			  (let ((mem (member fmla sforms
					     :key #'formula :test #'tc-eq)))
			    (if mem
				(car mem)
				(make-instance 's-formula 'formula fmla))))
		conjunct)
	      remaining-sforms)))

(defun compute-abstract-formula (formula-to-abstract list-state-predicates)
   (let* ((*pvs-bdd-hash* (make-hash-table
			  :hash-function 'pvs-sxhash :test 'tc-eq))
 	  (*bdd-pvs-hash* (make-hash-table))
	  (*history-bdd-hash-encoding* (make-hash-table
 		  :hash-function 'pvs-sxhash :test 'tc-eq))
          (*mu-nu-lambda-bindings-list* nil)
          (*make-abs-rec-app* nil)
          (*counter-possible-prf* 0)
          (*counter-attempt* 0)
          (is-a-good-list (is-a-good-list-of-pred? list-state-predicates)))
 (if is-a-good-list 
        (let ((*state-type* (domain (type (car is-a-good-list )))))
          (progn
          (create-abstract-state is-a-good-list)
          (setq *list-predicates* is-a-good-list)
          (alpha-image formula-to-abstract)
         ))
        nil )))
  

(defun is-a-good-list-of-pred? (list-state-predicates)
  (let* ((list-stringstate-predicates (if (listp list-state-predicates) 
					  list-state-predicates
					  (list list-state-predicates)))
	 (all-expr-pred-states (mapcar #'(lambda (pred)
					   (pc-typecheck
					       (pc-parse pred 'expr)))
				 list-stringstate-predicates))
	 (bad-pred-type (find-if-not #'(lambda (pred) (predtype? (type pred)))
			  all-expr-pred-states))
	 (bad-pred-freevars (find-if #'(lambda (pred) (freevars pred))
			      all-expr-pred-states)))
    (cond (bad-pred-type
	   (error-format-if "~a is not a predicate." bad-pred-type))
	  (bad-pred-freevars
	   (error-format-if "~a has free variables." bad-pred-freevars))
	  (t (let* ((all-type-pred-state (mapcar #'type all-expr-pred-states))
		    (all-state-types (remove-duplicates (mapcar #'domain
							  all-type-pred-state)
				       :test 'tc-eq)))
	       (cond ((cdr all-state-types)
		      (error-format-if
		       "State predicates have different domains"))
		     (t all-expr-pred-states)))))))
       


(defun is-a-good-state? (statetype)
  (recordtype? statetype))

;;
;;
;;

(defun create-abstract-state (list-state-predicates)
 (let* ((state-type *state-type*)
        (pred-expressions (mapcar #'(lambda (pred) (expression pred))
                                list-state-predicates))
        (abstracted-state-components
          (remove-duplicates
             (loop for x in pred-expressions
         	append (let ((result (free-state-components x)))
		               result))))
        (remaining-state-components (remove nil
             (mapcar #'(lambda (ff) 
                         (if (member (id ff) abstracted-state-components :test
                                            'tc-eq)
                               nil ff))
                                (fields state-type))))
        (good-remaining-state-components (remove nil
                 (mapcar #'(lambda (ff) 
                  (if (mu-translateable? (find-supertype (type ff))) ff nil))
                   remaining-state-components)))
        (abstract-fields
	 (let ((index 1))
	   (mapcar #'(lambda (avar)
		       (declare (ignore avar))
		       (let ((sym (makesym "B_~d" index)))
			 (loop while (member sym (fields state-type) :key #'id)
			       do (setq sym (makesym "B_~d" (incf index))))
			 (incf index)
			 (mk-field-decl sym *boolean* *boolean*)))
	     list-state-predicates)))
        (abstract-fields (append 
             good-remaining-state-components abstract-fields ))
        (abstract-type (typecheck (make-recordtype
				   (copy-list abstract-fields)))))
   (setq *abs-state-type* abstract-type)
   (setq *new-abstract-field-ids* (reverse (mapcar #'id abstract-fields)))
   (print-abstract-state-type list-state-predicates)))


(defun print-abstract-state-type (list-state-predicates)
  (let ((new-type *abs-state-type*))
    (pvs-message 
(format nil "~% The following abstract state is created: ~% ~%[# ~%~a#]~%" 
         (unparse-abstract-state-type new-type list-state-predicates)))))

(defun unparse-abstract-state-type (new-type list-state-predicates)
 (let* ((abstarct-fields (fields new-type))
        (rest-len (- (length abstarct-fields) (length list-state-predicates)))
        (new-list-pred (append list-state-predicates (make-list rest-len)))
        (string-fields-list (pairlis abstarct-fields new-list-pred))
        (string-fields-list (mapcar #'(lambda (one-pair)
                    (format nil "~a  ~a ~%" (unparse (car one-pair) :string t)
                         (if (cdr one-pair) 
                      (format nil "    ->  ~a " (unparse (cdr one-pair) :string t))
                                ""))) string-fields-list)))
   
   (format nil "~{~a~^~}" (reverse string-fields-list))))

;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;
;;
;; Main function: alpha-image: pvs-expr(c-state) -> pvs-expr (a-state)
;;
;;     computing alpha by descending through the boolean structure
;;      of the predicate.
;;     The context can be initialized by (bdd_1) (true) or with the
;;     alpha-image of a given invariant.
;;     sign is initialized with t (for over)
;;

(defun alpha-image (expr &optional invariant) ;; boolean expression
 (let ((time-now (get-universal-time)))
 (pvs-message (format nil "Starting computation of abstraction" ))
  (let* ((context (or invariant *true*))
         (computed-alpha-image (alpha-image-expr nil expr context)))
   (pvs-message (format nil "abstraction computed in ~d seconds~%" 
                  (- (get-universal-time) time-now )))
   (pvs-message (format nil 
  "calls to the decision procedure:~% possible: ~d   performed: ~d~2%"
   *counter-possible-prf* *counter-attempt*))
   computed-alpha-image)))


;; Should be broken into methods
(defun alpha-image-expr (sign expr context)
   (let ((expr (lift-state-bindings expr)))
       (alpha-image-expr* sign expr context)))

(defmethod alpha-image-expr*  (sign (expr expr) context)
 (let ((alpha-image-e
   (cond  ((any-mu-nu-expr-application? expr) 
                    (alpha-image-mu-nu-application sign expr  context))
          ((any-mu-nu-expr? expr) (alpha-image-mu-nu sign expr  context))
          ((mu-var-application? expr) 
                     (alpha-image-mu-var-application sign expr context))
          ((skolem-constant-state-expr? expr) 
                      (alpha-image-skolem-constant sign expr context))
          ((state-expr? expr) (alpha-image-state sign expr context))
          ((pred-state-expr? expr) (alpha-image-atom sign expr context))
          ((constant? expr) (alpha-image-constant sign expr context))
          (t 
               (if sign (alpha-image-atom t expr context)
                      (make-negation 
               (alpha-image-atom t (make-negation-with-p expr) context))
                  )))))
   alpha-image-e))

(defmethod alpha-image-expr* (sign (expr propositional-application) context)
  (let ((alpha-image-e
   (cond  ((disjunction? expr) 
               (alpha-image-disjunction sign expr  context))
          ((implication? expr) 
               (alpha-image-implication sign expr  context))
          ((conjunction? expr) 
               (alpha-image-conjunction sign expr  context))
          ((negation? expr) 
               (alpha-image-negation sign expr  context))
          ((iff-or-boolean-equation? expr) 
               (alpha-image-iff sign expr  context))
          ((branch? expr) 
               (alpha-image-branch sign expr  context))
          (t (call-next-method) )               
            )))
   alpha-image-e))

(defmethod alpha-image-expr* (sign (expr iff-or-boolean-equation) context)
 (alpha-image-iff sign expr  context))

(defmethod alpha-image-expr* (sign (expr branch) context)
 (alpha-image-branch sign expr  context))

(defmethod alpha-image-expr* (sign (expr binding-expr) context)
 (if (binding-state-expr? expr) 
                 (alpha-image-binding sign expr  context)
                 (call-next-method)   ))



(defun binding-state-expr? (expr)
 (if (binding-expr? expr)
      (every #'(lambda (x) (compatible-type (type x) 
                                  *state-type*)) (bindings expr))
     nil))

(defmethod expr-is-atom? ((expr propositional-application))
  nil)

(defmethod expr-is-atom? ((expr iff-or-boolean-equation))
  nil)

(defmethod expr-is-atom? ((expr branch))
  nil)

(defmethod expr-is-atom? ((expr binding-expr))
  nil)

(defmethod expr-is-atom? ((expr expr))
  (and (not (any-mu-nu-expr-application? expr)) 
       (and (not (any-mu-nu-expr? expr))
	    (and (not (mu-var-application? expr))
		 (not (pred-state-expr? expr))))))
;;

(defun any-mu-nu-expr? (expr)
  ;; /= from mu-nu-expr?. Without restriction to mu-translatable types
  (and (application? expr)
       (let ((op (operator expr)))
	 (and (typep op 'name-expr)
	      (memq (id op) '(|mu| |nu|))
	      (eq (id (module (declaration op))) '|mucalculus|)))))

(defun any-mu-nu-expr-application? (expr) 
  ;; /= from mu-nu-expr-application?. Without restriction to
  ;; mu-translatable types
  (and (application? expr)
       (let ((op (operator expr)))
	 (any-mu-nu-expr? op))))

(defun state-expr? (expr)
 (and (expr? expr) (compatible-type (type expr) *state-type*)))

(defun pred-state-expr? (expr)
 (and (expr? expr) (compatible-type (type expr) ( mk-predtype *state-type*))))

(defun skolem-constant-state-expr? (expr)
 (and
  (skolem-constant? expr) (compatible-type (type expr) *state-type*)))

;;
;; /= cases of expr
;;

(defun  alpha-image-disjunction (sign expr context)
  (let* ((bdd-args1  (alpha-image-expr* sign (args1 expr) context))
	 (bdd-args2  (alpha-image-expr* sign (args2 expr) context)))
   (make-disjunction (list bdd-args1 bdd-args2))))   
 
(defun  alpha-image-implication (sign expr context)
  (let ((fml (make-disjunction 
      (list (make-negation-with-p (args1 expr)) (args2 expr)))))
         (alpha-image-disjunction  sign fml context)))
 
(defun  alpha-image-conjunction (sign expr context)
 (let* ((bdd-args1  (alpha-image-expr* sign (args1 expr)  context))
        (updated-context 
             (mk-conjunction (list context  (args1 expr))))
        (bdd-args2  (alpha-image-expr* sign (args2 expr) 
                                updated-context)))
   (mk-conjunction (list bdd-args1 bdd-args2))))

(defun  alpha-image-negation (sign expr context)
 (let ((bdd-args1 (alpha-image-expr* (not sign) (args1 expr) context)))
  (make-negation-with-p  bdd-args1))) 
    
(defun  alpha-image-iff (sign expr context)
  (let ((fml1 (make!-implication (args1 expr) (args2 expr)))
        (fml2 (make!-implication (args2 expr) (args1 expr))))
  (alpha-image-conjunction 
          sign (make!-conjunction fml1 fml2) context)))

(defun  alpha-image-branch (sign expr context)
   (let* ((bdd-cond (condition expr))
          (bdd-then (then-part expr))
          (bdd-else (else-part expr))
          (dis-expr (make-disjunction
            (list (mk-conjunction (list bdd-cond bdd-then)) 
                  (mk-conjunction (list (make-negation bdd-cond)
                                              bdd-else)))))) 
    (alpha-image-expr* sign dis-expr context)))
           
(defun alpha-image-binding (sign expr  context)
 (let ((good-type-bindings (every #'(lambda (x) (compatible-type (type x) 
                                  *state-type*)) (bindings  expr))))
                              
   (if  good-type-bindings
            (cond ((forall-expr? expr) (alpha-image-forall sign expr context))
                  ((exists-expr? expr) (alpha-image-exists sign expr context))
                  ((lambda-expr? expr) (alpha-image-lambda sign expr context))
                  )
       expr)))

(defun lift-state-bindings (expr)
 (if (binding-expr? expr)
        (let* ((bindings-var  (bindings expr))
               (state-bindings (remove nil 
                (mapcar #'(lambda (x) (if (compatible-type (type x) 
                                  *state-type*) x nil)) bindings-var)))
              (other-bindings (remove nil 
              (mapcar #'(lambda (x) (if (not (compatible-type (type x) 
                                  *state-type*)) x nil)) bindings-var))))
            (cond ((forall-expr? expr) 
 (my-make-forall-expr state-bindings other-bindings expr))
                  ((exists-expr? expr)  
 (my-make-exists-expr state-bindings other-bindings expr))
                  ((lambda-expr? expr) 
 (my-make-lambda-expr state-bindings other-bindings expr))
 (t expr))) expr))



(defun my-make-forall-expr (state-bindings other-bindings expr)
 (if other-bindings
      (if state-bindings (make-forall-expr state-bindings 
                             (make-forall-expr other-bindings (expression expr)))
           expr)
     expr))

(defun my-make-exists-expr (state-bindings other-bindings expr)
 (if other-bindings
      (if state-bindings (make-exists-expr state-bindings 
                             (make-exists-expr other-bindings (expression expr)))
           expr)
     expr))

(defun my-make-lambda-expr (state-bindings other-bindings expr)
 (if other-bindings
      (if state-bindings (make-lambda-expr state-bindings 
                             (make-lambda-expr other-bindings (expression expr)))
           expr)
     expr))


(defun alpha-image-forall (sign expr context)
  (let* ((exprargs (expression expr))
         (state-bindings
	  (remove nil
		  (mapcar #'(lambda (x)
			      (if (compatible-type (type x) 
						   *state-type*) x nil))
		    (bindings  expr))))
         (abs-state-bindings (mapcar #'(lambda (x) 
					 (make-abstract-state-bindings-var x))
			       state-bindings))
         (*bound-variables* (append abs-state-bindings *bound-variables*))
         (*assoc-bindings* 
	  (append (pairlis state-bindings abs-state-bindings)
		  *assoc-bindings*))
         (abstract-expr (alpha-image-expr* sign                   
					   exprargs context))
         (concrete-expr (typecheck  
			    (mk-forall-expr abs-state-bindings  
			      abstract-expr))))
    concrete-expr))

(defun alpha-image-exists (sign expr context)
  (let* ((exprargs (expression expr))
         (state-bindings (remove nil (mapcar #'(lambda (x) (if 
                 (compatible-type (type x) 
                                  *state-type*) x nil)) (bindings  expr))))
         (abs-state-bindings (mapcar #'(lambda (x) 
               (make-abstract-state-bindings-var x)) state-bindings))
         (*bound-variables* (append abs-state-bindings *bound-variables*))
         (*assoc-bindings* 
	  (append (pairlis state-bindings abs-state-bindings) *assoc-bindings*))
         (abstract-expr (alpha-image-expr* sign                            
                              exprargs context))
         (concrete-expr (typecheck
                  (mk-exists-expr abs-state-bindings  
                                abstract-expr))))
    concrete-expr))

(defun alpha-image-lambda (sign expr context)
  (let* ((exprargs (expression expr))
         (state-bindings
	  (remove nil
		  (mapcar #'(lambda (x)
			      (when (compatible? (type x) *state-type*)
				x))
		    (bindings  expr))))
         (abs-state-bindings (mapcar #'(lambda (x) 
               (make-abstract-state-bindings-var x)) state-bindings))
         (*bound-variables* (append abs-state-bindings *bound-variables*))
         (*assoc-bindings* 
	  (append (pairlis state-bindings abs-state-bindings) *assoc-bindings*)))
        (typecheck (mk-lambda-expr  abs-state-bindings   
              (alpha-image-expr* sign exprargs context)))))



(defun alpha-image-mu-nu-application (sign expr  context)
  (let* ((exprargs (args1 expr))
         (munuop (operator expr))
         (concrete-expr (make-application (alpha-image-mu-nu sign munuop  context)
			  (alpha-image-expr* sign exprargs  context))))
    concrete-expr))

(defun alpha-image-mu-nu (sign expr context)
  (let* ((muargs1bindgs (bindings (args1 expr))) 
         (munuop (mk-name-expr (id (operator expr))))
         (*mu-nu-lambda-bindings-list*
	      (append muargs1bindgs
		      *mu-nu-lambda-bindings-list*))
         (mu-abs-bindings 
           (mapcar #'make-abstract-pred-state-bindings-var muargs1bindgs))
         (*bound-variables* (append  mu-abs-bindings *bound-variables*))
         (lambda-e (typecheck (mk-lambda-expr  mu-abs-bindings 
				(alpha-image-expr* sign
						   (expression (args1 expr))
						   context))))
         (concrete-expr (typecheck 
			    (mk-application munuop lambda-e))))
   concrete-expr))

(defun mu-var-application? (expr)
  (when (and (application? expr)
	     (name-expr? (operator expr)))
    (let* ((op (operator expr))
	   (is-mu-var? (memq (declaration op) *mu-nu-lambda-bindings-list*))
	   (muvarargs (arguments expr)))
      (and is-mu-var?
	   (variable? (car muvarargs)) 
	   (compatible? *state-type* (type (car muvarargs)))))))


(defun alpha-image-mu-var-application (sign expr context)
  (declare (ignore sign context))
  (let* ((op (operator expr))
	 (muvarargs (car (arguments expr)))
	 (new-op (make-abstract-pred-state-var op))
	 (new-var (make-abstract-state-var muvarargs))
	 (concrete-expr (mk-application new-op new-var)))
    concrete-expr))
        
(defun alpha-image-state (sign expr context)
 (cond ((variable? expr) (id-substit nil nil expr))
       ((record-expr? expr) (alpha-image-record sign expr context))))

(defun alpha-image-skolem-constant (sign expr context)
  (declare (ignore sign context))
 (let* ((new-id (makesym (format nil "sko_~a" (string-right-trim "!1" 
                           (string (id expr))))))
         (type-abstract-var *abs-state-type*)
         (new-abs-var-decl (mk-const-decl new-id type-abstract-var))
         (new-sko-var (mk-name-expr new-abs-var-decl)))
   new-sko-var))
    
(defun alpha-image-record (sign expr context)
  (alpha-image-atom sign expr context))

(defun alpha-image-constant (sign expr context)
 (alpha-image-atom sign expr context))

;;
;;
;;
  



(defun make-abstract-state-var (concrete-var)
  (let* ((abstract-name-var (makesym "abs_~a" (id concrete-var)))
	 (new-var (mk-name-expr abstract-name-var)))
    new-var))


(defun make-abstract-state-bindings-var (concrete-bind-var)
  (let* ((abstract-name-var (makesym "abs_~a"
				     (id concrete-bind-var)))
	 (type-abstract-var *abs-state-type*)
	 (new-abs-var-decl (make-bind-decl abstract-name-var type-abstract-var)))
    new-abs-var-decl))


(defun make-abstract-pred-state-var (concrete-var)
  (let* ((abstract-name-var (makesym "abs_~a"
				     (id concrete-var) ))
        (new-var (mk-name-expr abstract-name-var)))
 new-var
   ))
   
(defun make-abstract-pred-state-bindings-var (concrete-bind-var)
   (let* ((abstract-name-var (makesym  "abs_~a"
				       (id concrete-bind-var)))
        (type-abstract-var (mk-predtype *abs-state-type*))
        (new-abs-var-decl (make-bind-decl abstract-name-var type-abstract-var)))
    new-abs-var-decl)) 


;;
;;
;;
;;;;;;;;;;;;;;;;;;
;;
;;
;;

(defun alpha-image-atom (over? expr context)
 (let* ((free-vars (freevars expr))
        (free-state-vars (remove nil (mapcar #'(lambda (x) (if 
                            (compatible-type (type x) *state-type*) x nil)) free-vars)))
        (*state-var-1* (let ((elem (updated-state-var expr)))
                         (if elem elem (car free-state-vars))))
        (*state-var-2* (car (set-difference free-state-vars 
                   (list *state-var-1*) :test 'same-declaration )))
        (*abs-state-var-1* (if *state-var-1* 
                             (make-abstract-state-var *state-var-1*) nil ))
        (*abs-state-var-2* (if *state-var-2* 
                                (make-abstract-state-var  *state-var-2*) nil))
         (computed-image
          (if (null free-vars)
              (alpha-image-atom-constant over? expr context) 
           (let ((list-predicates 
                      (create-current-list-pred *state-var-1*))
                 (list-primed-predicates 
                      (create-current-list-pred *state-var-2*)))
   (basic-alpha-image-atom over? expr list-predicates list-primed-predicates 
                           context)))))
     computed-image))


(defun normalize-equality-expr (expr)
  (if (equation? expr)
      (let* ((args (arguments expr))
             (fml-update (member-if  #'(lambda (x) (update-expr? x)) args))
             (fml-no-u (member-if  #'(lambda (x) (not(update-expr? x))) args))
             (fml-update (if fml-update (car fml-update) nil))
             (fml-no-u (if fml-no-u (car fml-no-u) nil)))
        (if (or (null fml-update) (null fml-no-u)) expr
	    (normalize-update-expr fml-no-u fml-update)))
      expr))

(defun normalize-update-expr (fml-no-u fml-update)
 (let* ((exp (expression fml-update))
        (updates (assignments  fml-update))
        (updates (mapcar #'(lambda (one-assign) 
                     (make!-update-expr exp (list one-assign))) updates))
        (new-exp-list (mapcar #'(lambda (one-expr)
                        (make-equality fml-no-u one-expr)) updates)))
 (make-conjunction new-exp-list)))


(defun alpha-image-atom-constant (over? expr context)
  (declare (ignore over? context))
  expr)

(defun create-current-list-pred (state-var)
 (if (null state-var) nil
 (let* ((list-pred (mapcar #'expression *list-predicates*))
        (current-list (mapcar #'(lambda (pred) 
                           (struc-substit state-var (car (freevars pred)) pred))
                            list-pred)))
 current-list)))

;;
;;
;;
;;

(defun struc-substit (new-var state-var expr)
 (substit expr (acons (declaration state-var) new-var nil)))

(defun id-substit (new-var state-var expr)
  (declare (ignore new-var state-var))
 (substit (typecheck expr) *assoc-bindings*))

;;;;;;;;;;;;;;;;;;
;;
;; gamma image
;;

(defun local-gamma-image (bdd-expr)
  (let ((*bool-hash-table* *abstract-bdd-pvs-hash*))
     (real-local-gamma-image bdd-expr)))

(defun local-concretize  (bdd-expr)
 (let ((*make-abs-rec-app* t))
   (local-gamma-image bdd-expr)))

(defun gamma-image (bdd-expr)
  (let ((*bool-hash-table* *bdd-pvs-hash*))
     (real-gamma-image bdd-expr)))

(defun gamma-image-contex (bdd-expr)
  bdd-expr)

;; new relative to the new handeling of contex.
;;

;;
;;

(defun real-gamma-image (bdd-expr) 
  (if (expr? bdd-expr) bdd-expr
      (let ((bdd-expr bdd-expr)) 
	(if (bdd-1? bdd-expr) *true*
	    (let* ((pvs-list-expressions (gamma-translate-bdd-to-pvs bdd-expr))
		   (pvs-list-of-disj (mapcar #'(lambda (list-conj) 
						 (mk-conjunction list-conj))
				       pvs-list-expressions ))
		   (pvs-expression (make-disjunction pvs-list-of-disj)))
	      pvs-expression)))))

(defun real-local-gamma-image (bdd-expr) 
  (if (expr? bdd-expr) bdd-expr
      (let ((bdd-expr bdd-expr)) 
	(if (bdd-1? bdd-expr) *true*
	    (let* ((pvs-list-expressions (local-gamma-translate-bdd-to-pvs
					  bdd-expr))
		   (pvs-list-of-disj (mapcar #'(lambda (list-conj) 
						 (mk-conjunction list-conj))
				       pvs-list-expressions ))
		   (pvs-expression (make-disjunction pvs-list-of-disj)))
	      pvs-expression)))))

(defun gamma-translate-bdd-to-pvs (bdd-expr)
   (let ((list-of-conjuncts 
          (translate-from-bdd-list  (bdd_sum_of_cubes bdd-expr 1))))
     (gamma-from-bdd-list-to-pvs-list list-of-conjuncts)))

(defun local-gamma-translate-bdd-to-pvs (bdd-expr)
   (let ((list-of-conjuncts 
          (translate-from-bdd-list  (bdd_sum_of_cubes bdd-expr 1))))
     (local-gamma-from-bdd-list-to-pvs-list list-of-conjuncts)))

(defun gamma-from-bdd-list-to-pvs-list (list-of-conjuncts)
(let* ((abstract-bdd-pvs-hash *bool-hash-table*)
       (lit-list 
            (mapcar #'(lambda (conj)
	       (mapcar #'(lambda (lit)
	           (if (consp lit)
		       (make-negation 
                           (gethash (car lit) abstract-bdd-pvs-hash))
                           (gethash lit abstract-bdd-pvs-hash)))
		      	   conj))
		   list-of-conjuncts)))
    lit-list))

(defun local-gamma-from-bdd-list-to-pvs-list (list-of-conjuncts)
  (let* ((abstract-bdd-pvs-hash *bool-hash-table*)
	 (lit-list 
	  (mapcar #'(lambda (conj)
		      (mapcar #'(lambda (lit)
				  (get-abstract-encoding
				   lit abstract-bdd-pvs-hash))
			conj))
	    list-of-conjuncts)))
    lit-list))

(defun get-abstract-encoding (lit abstract-bdd-pvs-hash)
  ;; (pvs-message (format nil "var1: ~a    var2: ~a"
  ;; (unparse *abs-state-var-1* :string t)
  ;; (unparse *abs-state-var-2* :string t)))
  (if (or (not *make-abs-rec-app*)
	  (> (if (consp lit) (car lit) lit)
             (* 2 (length *list-indices*))))
      (if (consp lit)
	  (make-negation 
	   (gethash (car lit) abstract-bdd-pvs-hash))
	  (gethash lit abstract-bdd-pvs-hash))
      (let* ((neg? (consp lit))
             (ll (length *list-indices*))
             (lit (if (consp lit) (car lit) lit))
             (recgnzr (format nil "~a (~a(~a))" 
			(if neg? "not" "")  
			(nth (1- (if (< ll lit) (- lit ll) lit))
			     *new-abstract-field-ids*)
			(if (< ll lit)
			    (unparse *abs-state-var-2* :string t)
			    (unparse *abs-state-var-1* :string t))
                        )))
	(typecheck(pc-parse recgnzr 'expr ) :expected *boolean*))))
     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;
;; Alpha-image atoms....
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Given a set of predicate phi_1,...,phi_k defining
;; a partition of the set of concrete states, compute
;; the abstraction alpha()
;;

(defun already-abs? (predicate)
 (gethash predicate *history-bdd-hash-encoding*))



;;;
;;
;;;;;;;;;;;;;;;;;;;

(defun basic-alpha-image-atom (over? predicate list-predicates 
                                 list-primed-predicates context)
  (let* (
         (*pvs-abstract-bdd-hash* (make-hash-table
 			  :hash-function 'pvs-sxhash :test 'tc-eq))
 	 (*abstract-bdd-pvs-hash* (make-hash-table))
         (*abstract-bool-var-hash* (make-hash-table))
         (*list-indices* (make-list-of-indices (length list-predicates)))
         (nb-predicate (length *list-indices*))
         (*list-primed-indices* (mapcar #'(lambda (indice) 
                (+ indice nb-predicate)) *list-indices*))
         (*primed-exclusive* (make-bdd-exclusivity *list-primed-indices*))
         (*unprimed-exclusive* (make-bdd-exclusivity *list-indices*)))
   (create-hash-tables-for-bool-vars list-predicates list-primed-predicates)
   (init-primed-unprimed-variables list-predicates list-primed-predicates)
   (setq *proof-counter* 0) 
   (let*   ((bdd_alpha_image (let ((already-computed (already-abs? predicate)))
 (if already-computed already-computed
 (let* ((list-primed-unprimed-dep-list 
                  (get-list-of-dependent-indices predicate))
        (*list-not-affected* (set-difference *list-indices*
                     (car list-primed-unprimed-dep-list)))
        (*dep-list-indices* (car list-primed-unprimed-dep-list))
        (*dep-list-primed-indices* (cdr list-primed-unprimed-dep-list))
        (not-affected? (and (null *dep-list-indices*) 
                            (null *dep-list-primed-indices*)))
        (primed? (depends-on-only-primed? predicate))
        (unprimed? (depends-on-only-unprimed? predicate))
        (unchanged-expr (if (or primed? unprimed?) *true* 
           (create-unchanged-expr *list-not-affected* predicate)))
        (decomposed-conj (conjuncts (normalize-equality-expr predicate)))
        (not-affec-assign (remove nil (mapcar #'(lambda (one-c) (let ((dep-i
                              (get-list-of-dependent-indices one-c)))
                        (if (and (null (car dep-i)) (null (cdr dep-i)))
                              one-c nil)))   decomposed-conj)))
        (bdd_image_abs_var_not_aff (if (and (not (or primed? unprimed?))
            (not not-affected?))  (mk-conjunction
                (mapcar #'(lambda (one-c) 
                   (alpha-image-atom-not-affected over? one-c context))
                 not-affec-assign)) *true* ))
        (computed-image
 (if not-affected? (alpha-image-atom-not-affected over? predicate context)
 (if over?
  (cond (primed? ;; over
            (real-compute-alpha-image-atom-over predicate t  context))
        (unprimed?
            (real-compute-alpha-image-atom-over predicate nil context))
        (t (real-compute-mixed-alpha-image-atom-over predicate context)))
  (cond (primed?
            (real-compute-alpha-image-atom-under predicate t context))
        (unprimed? ;; under
            (real-compute-alpha-image-atom-under predicate nil context))
        (t (real-compute-mixed-alpha-image-atom-under predicate context))))))
         (computed-image (mk-conjunction 
			  (list  computed-image bdd_image_abs_var_not_aff)))
         (computed-image (cons computed-image unchanged-expr))   
          )
  (setf (gethash predicate *history-bdd-hash-encoding*) 
	computed-image)
  computed-image ))))
          (abstract-image (car bdd_alpha_image)))
     (mk-conjunction  (list abstract-image (cdr bdd_alpha_image)))
)))


;;
;;
;;
;;;;;;;;;;;;;;;;;;
;;
;;


;; init primed and unprimed list of variables
(defun init-primed-unprimed-variables (list-predicates list-primed-predicates)
   (setq *primed-variables* nil)
   (setq *unprimed-variables* nil)
  (let ((list-unprimed-vars 
            (mapcar #'(lambda (expr) (freevars expr)) list-predicates))
        (list-primed-vars 
            (mapcar #'(lambda (expr) (freevars expr)) list-primed-predicates)))
    (dolist (one-list list-primed-vars)
      (setq *primed-variables* (append *primed-variables* one-list)))
    (dolist (one-list list-unprimed-vars)
      (setq *unprimed-variables* (append *unprimed-variables* one-list)))
     (setq *primed-variables* (remove-duplicates *primed-variables* 
                                              :test 'tc-eq))
     (setq *unprimed-variables* (remove-duplicates *unprimed-variables* 
                                              :test 'tc-eq))))


;; initialize hash-tables
(defun create-hash-tables-for-bool-vars 
    (list-predicates list-primed-predicates)
 (let ((list-unprimed-pair (pairlis *list-indices* list-predicates))
       (list-primed-pair
	(when list-primed-predicates
	  (pairlis *list-primed-indices* list-primed-predicates))))
  (dolist (one-pair list-unprimed-pair) ;; unprimed-predicates
     (let ((varid (car one-pair))
           (predicate (cdr one-pair)))
             (setf (gethash predicate *pvs-abstract-bdd-hash*) varid)
             (setf (gethash varid *abstract-bdd-pvs-hash*) predicate)))
  (dolist (one-pair list-primed-pair) ;; primed-predicates
     (let ((varid (car one-pair))
           (predicate (cdr one-pair)))
             (setf (gethash predicate *pvs-abstract-bdd-hash*) varid)
             (setf (gethash varid *abstract-bdd-pvs-hash*) predicate)))
 (setf (gethash *true* *pvs-abstract-bdd-hash*) *true*)))


(defun alpha-image-atom-not-affected (over? predicate context)
  (declare (ignore context))
 (let* ((vars (freevars predicate))
        (state-var (if (null vars) nil (car vars)))
        (pair-state-vars (if (cdr vars) (list (car vars) (car(cdr vars)))
                                   nil ))
        (state-var1 (if pair-state-vars (car pair-state-vars) nil))
        (state-var2 (if pair-state-vars (car (cdr pair-state-vars)) nil))
        (free-comp (get-free-state-components predicate))
        (abstract-fields (mapcar #'id (fields *abs-state-type*)))
        (all-comp-finite (set-difference free-comp abstract-fields 
                                      :test #'equal))
        (all-comp-finite? (null all-comp-finite))
        (bdd-varid (fresh-bdd-varid))
        (new-expression 
	 (if (null state-var) 
	     predicate
	     (if (not all-comp-finite?)
		 (if over? *true* *false*);; what about the hash value??
		 (if (null pair-state-vars)
		     (let* ((new-var (make-abstract-state-var state-var))
			    (new-expr (id-substit new-var state-var predicate)))
		       new-expr)
		     (let* ((predicate (decompose-assign-predicate predicate))
			    (new-var1 (make-abstract-state-var 
				       (nth 0 pair-state-vars)))
			    (new-var2 (make-abstract-state-var 
				       (nth 1 pair-state-vars))) 
			    (new-expr (id-substit new-var1 state-var1 predicate))
			    (new-expr (id-substit new-var2 state-var2 new-expr)))
		       new-expr))))))
   (bdd_create_var bdd-varid)
   (setf (gethash bdd-varid *abstract-bdd-pvs-hash*) new-expression)
   (if (and (not (null state-var)) (not all-comp-finite?)) ;;(bdd_1) 
       (if over? *true* *false*)  ;; what about the hash value??
       new-expression)))
                               

(defun decompose-assign-predicate (predicate)  
  (let* ((new-fml (normalize-decompose-equality predicate))
	 (changed? (not (tc-eq predicate new-fml))))
    (if changed?
	(let ((fml (mapcar #'eliminate-eq-conjunct
		     (conjuncts new-fml))))
	  (make-conjunction fml))
	predicate)))

(defun  eliminate-eq-conjunct (conj)    
  (if (equation? conj) 
      (let* ((fml1 (args1 conj))
	     (fml2 (args2 conj))
	     (field1? (field-application? fml1))
	     (field2? (field-application? fml2)))
	(if (and field1? field2?
		 (eq (id fml1) (id fml2)))
	    *true*
	    conj))
      conj))
        

;;
;; 
;;

(defun real-compute-alpha-image-atom-over (predicate primed? context)
  (let* (;;(bdd_alpha (bdd_1)) ;; initialize alpha with true
         (bdd_alpha *unprimed-exclusive*) ;; initialize alpha with exclusive
         (bdd_list_previous_fail (list (bdd_0)))  ;; initialize fail with false
         (bdd_list_current_fail nil )
         (force-stop nil) ;; stop when exact abstraction or any other criterion
         (generated_bool_exprs 
              (generate-possible-disj-bool-expr bdd_alpha 
               bdd_list_previous_fail (list (bdd_0))  primed?))
      ;;  (generated_bool_exprs (reverse generated_bool_exprs))) ;; no reverse 
         (generated_bool_exprs generated_bool_exprs))
   (loop while (and (not force-stop) (not (null generated_bool_exprs))) do
       (dolist (one_bool_expr generated_bool_exprs)
          (if force-stop (setq force-stop force-stop) ;;stop looping
         (if (disj-subsumed-by-alpha 
                    one_bool_expr bdd_alpha bdd_list_previous_fail)
              (setq bdd_list_current_fail (cons one_bool_expr 
                                                bdd_list_current_fail))

            (let* ((pvs-expr (local-gamma-image one_bool_expr))
                   (is-exact-abstraction? (same-expression pvs-expr predicate))
                   (context-expr (gamma-image-contex context)))
            (if is-exact-abstraction?
                 (progn 
                  (setq force-stop t)
                  (setq bdd_alpha (bdd_and bdd_alpha one_bool_expr))
                  )
          (if  (call-decision-procedure-and-prove 
                         (mk-conjunction (list context-expr predicate))
                                  pvs-expr)
                (setq bdd_alpha (bdd_and bdd_alpha one_bool_expr))
                (setq bdd_list_current_fail 
                            (cons one_bool_expr bdd_list_current_fail))
                )))
          )))
       (if (or force-stop (> *proof-counter* *max-proof-counter* ))
                 (setq force-stop t) ;;stop looping
        (progn
        (setq generated_bool_exprs (if 
                    (or (> *proof-counter* *max-proof-counter* )  
                                  (null bdd_list_current_fail)) 
            nil
             (generate-possible-disj-bool-expr bdd_alpha 
                  bdd_list_previous_fail   bdd_list_current_fail primed?)))
        (setq bdd_list_previous_fail bdd_list_current_fail)
        (setq bdd_list_current_fail nil)
      )))
 (local-concretize bdd_alpha)))

(defun real-compute-alpha-image-atom-under (predicate primed? context)
  (make-negation (real-compute-alpha-image-atom-over 
	    (make-negation-with-p predicate) primed? context)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
(defun get-list-of-dependent-indices (predicate)
 (let ((free-vars-pred (free-state-components predicate))
       (unprimed-indices *list-indices*)
       (primed-indices *list-primed-indices*))
  (construct-dep-abs-vars 
        free-vars-pred unprimed-indices primed-indices  nil nil)))

(defun construct-dep-abs-vars (free-vars-pred 
   unprimed-indices primed-indices accuml-unprimed accuml-primed)
 (if (null unprimed-indices)
       (set-dependant-indice-var accuml-unprimed accuml-primed)
  (let* ((varid (car unprimed-indices))
         (p-varid (car primed-indices))
         (pred-unprimed (gethash varid  *abstract-bdd-pvs-hash*))
         (free-vars-abs-var (free-state-components pred-unprimed))
         (depends (intersection free-vars-pred free-vars-abs-var)))
   (if depends
     (construct-dep-abs-vars 
           free-vars-pred (cdr unprimed-indices)  (cdr primed-indices)
               (cons varid  accuml-unprimed) (cons p-varid accuml-primed))
     (construct-dep-abs-vars 
           free-vars-pred (cdr unprimed-indices)  (cdr primed-indices)
              accuml-unprimed  accuml-primed)))))
      
 
(defun set-dependant-indice-var (accuml-unprimed accuml-primed)
 (let ((accuml-unprimed (reverse accuml-unprimed))
       (accuml-primed (reverse accuml-primed))) 
 (cons accuml-unprimed accuml-primed)))


(defun make-list-of-all-literals (primed?)
 (if primed? 
  (mapcar #'(lambda (varid) (bdd_create_var varid)) *dep-list-primed-indices*)
  (mapcar #'(lambda (varid) (bdd_create_var varid)) *dep-list-indices*)))

;;
;;
;;

(defun generate-possible-disj-bool-expr (bdd_alpha previous-list
						   current-list primed?)
  (declare (ignore previous-list))
   (let* ((list-literals (make-list-of-all-literals primed?))
         (dis-with-lit list-literals)
         (dis-with-neg-lit (mapcar #'(lambda (bdd) (bdd-not bdd)) 
                               list-literals))
         (*intermediate-list* nil))
      (dolist (one-old-bool-expr current-list)
            (let ((list-lit  (remove nil
                (mapcar #'(lambda (bdd)
                            (build-disj bdd_alpha bdd one-old-bool-expr))
                              dis-with-lit)))
                  (list-neg-lit (remove nil
               (mapcar #'(lambda(bdd)
                           (build-disj bdd_alpha  bdd one-old-bool-expr))
                              dis-with-neg-lit))))
           (setq *intermediate-list* 
             (append (append list-lit list-neg-lit) *intermediate-list*))))
   (setq *intermediate-list* 
    (mapcar #'(lambda (possible_disj_bdd)
         (if (or (bdd-1? (bdd_implies bdd_alpha possible_disj_bdd))
                 (bdd-1? (bdd_implies bdd_alpha (bdd_not possible_disj_bdd))))
                  nil 
                             possible_disj_bdd)) *intermediate-list* ))
  (remove nil  (remove-duplicates *intermediate-list* :test 'equal))))
                   
;;
;;

(defun generate-possible-conj-bool-expr (bdd_alpha previous-list
						   current-list primed?)
  (declare (ignore previous-list))
   (let* ((list-literals (make-list-of-all-literals primed?))
         (dis-with-lit list-literals)
         (dis-with-neg-lit (mapcar #'(lambda (bdd) (bdd-not bdd)) 
                               list-literals))
         (*intermediate-list* nil))
      (dolist (one-old-bool-expr current-list)
            (let ((list-lit  (remove nil
                (mapcar #'(lambda (bdd)
                            (build-conj bdd_alpha bdd one-old-bool-expr))
                              dis-with-lit)))
                  (list-neg-lit (remove nil
               (mapcar #'(lambda(bdd)
                           (build-conj bdd_alpha  bdd one-old-bool-expr))
                              dis-with-neg-lit))))
           (setq *intermediate-list* 
             (append (append list-lit list-neg-lit) *intermediate-list*))))
   (setq *intermediate-list* 
    (mapcar #'(lambda (possible_conj_bdd)
         (if (or (bdd-1? (bdd_implies possible_conj_bdd bdd_alpha ))
                 (bdd-1? (bdd_implies (bdd_not possible_conj_bdd) bdd_alpha)))
                  nil 
                             possible_conj_bdd)) *intermediate-list* ))
  (remove nil  (remove-duplicates *intermediate-list* :test 'equal))))
                   
;;
;;
;;
;;

(defun build-disj (bdd_alpha bdd one-old-bool-expr)
 (if (bdd-0? one-old-bool-expr) (bdd_or bdd one-old-bool-expr)
 (let* ((new_bdd (bdd_or bdd one-old-bool-expr)) ;; new expr to test
        (impl_alpha_new (bdd_implies bdd_alpha new_bdd)) ;; subsumed by alpha
        (impl_alpha_old (bdd_implies bdd_alpha one-old-bool-expr))
        ;;(old_implies_new (bdd_implies one-old-bool-expr new_bdd)) ;; redunduncy
        (equiv_and_new (bdd_equiv (bdd_and new_bdd bdd_alpha)
                                  (bdd_and bdd bdd_alpha)))
        (equiv_old_new (bdd_equiv impl_alpha_new impl_alpha_old)))

 (if (or (bdd-1? impl_alpha_new) 
         (or (bdd-1? equiv_old_new)
             (or (bdd-1? equiv_and_new)
                 (bdd-1? impl_alpha_old))))
      nil new_bdd))))


(defun build-conj (bdd_alpha bdd one-old-bool-expr)
 (if (bdd-1? one-old-bool-expr) (bdd_and bdd one-old-bool-expr)
 (let* ((new_bdd (bdd_and bdd one-old-bool-expr)) ;; new expr to test
        (impl_alpha_new (bdd_implies new_bdd bdd_alpha)) ;; subsumed by alpha
        (impl_alpha_old (bdd_implies one-old-bool-expr bdd_alpha))
        ;;(old_implies_new (bdd_implies new_bdd one-old-bool-expr )) ;; redunduncy
        (equiv_and_new (bdd_equiv (bdd_or new_bdd bdd_alpha)
                                  (bdd_or bdd bdd_alpha)))
        (equiv_old_new (bdd_equiv impl_alpha_new impl_alpha_old)))
 (if (or (bdd-1? impl_alpha_new) 
         (or (bdd-1? equiv_old_new)
             (or (bdd-1? equiv_and_new)
                 (bdd-1? impl_alpha_old))))
      nil new_bdd))))
;;
;;
;;
;; Mixed atoms.
;;
;;
;;

;;;;;;;;;;;;;;;;;;;;;
;;
;;


(defun real-compute-mixed-alpha-image-atom-over (predicate  context)
   (let* (;;(bdd_alpha (bdd_1)) ;; initialize alpha with true
          (bdd_alpha (bdd_and *unprimed-exclusive* *primed-exclusive*))
          (primed_bdd_list_previous_fail (list (bdd_0)))  ;; initialize fail with false
          (primed_bdd_list_current_fail nil)
          ;;(unprimed_bdd_list_previous_fail (list (bdd_1)))  ;; initialize fail with true
          ;;(unprimed_bdd_list_current_fail nil)
          (history_list_of_failed_primed_disj nil)
          (generated_primed_bool_exprs (reverse ;; no reverse !
              (generate-possible-disj-bool-expr bdd_alpha 
               primed_bdd_list_previous_fail (list (bdd_0))  t)))

           (generated_unprimed_bool_exprs 
             (generate-possible-disj-bool-expr (bdd_1)
               (list (bdd_0)) (list (bdd_0)) nil)))
 ;; test "primed disjunctions", but only disj in the context
   (loop while (not (null generated_primed_bool_exprs)) do
       (dolist (one_bool_expr generated_primed_bool_exprs)
          (if (disj-subsumed-by-alpha 
                    one_bool_expr bdd_alpha primed_bdd_list_previous_fail)
              (setq primed_bdd_list_current_fail (cons one_bool_expr 
                                                primed_bdd_list_current_fail))
            (let ((pvs-expr (local-gamma-image one_bool_expr))
                  (context-expr (gamma-image-contex context)))
          (if  (call-decision-procedure-and-prove 
                         (mk-conjunction (list context-expr predicate)) 
                                  pvs-expr)
                (setq bdd_alpha (bdd_and bdd_alpha one_bool_expr))
                (setq primed_bdd_list_current_fail 
                           (cons one_bool_expr primed_bdd_list_current_fail))
                ))
          ))
           ;; begin adding ant...subsumed-by-alpha (bool_expr bdd_fail bdd_alpha)
         (let ((generated_unprimed_bool_exprs 
                  (only-antecedants-in-context 
                     generated_unprimed_bool_exprs context))
                ;;(context-expr (gamma-image-contex context))
                (add-primed_bdd_list_current_fail (remove nil
                (mapcar #'(lambda(one-bdd) (if 
                 (subsumed-by-alpha one-bdd (bdd_0)  bdd_alpha) nil one-bdd))
                     primed_bdd_list_current_fail ))))
        (dolist (one-fail add-primed_bdd_list_current_fail)
         (dolist (one-antecedant generated_unprimed_bool_exprs)
          (let ((pvs-ant-expr (local-gamma-image one-antecedant))
                (pvs-expr (local-gamma-image one-fail)))
            (if (call-decision-procedure-and-prove 
              (mk-conjunction (list pvs-ant-expr ;;context-expr ;; to check if
                                                                ;; reachable
                     predicate)) pvs-expr)
                (setq bdd_alpha (bdd_and bdd_alpha 
                    (bdd_implies one-antecedant one-fail)))
                (setq primed_bdd_list_current_fail 
                   (cons one-fail primed_bdd_list_current_fail)))))))
              ;; end adding ant...
        (setq generated_primed_bool_exprs nil )
        (setq primed_bdd_list_previous_fail primed_bdd_list_current_fail)
        (setq history_list_of_failed_primed_disj 
                (append history_list_of_failed_primed_disj primed_bdd_list_current_fail))
        (setq primed_bdd_list_current_fail nil)
      )
  ;; test "unprimed conjunctiosn => failed primed disjunction"
  ;; failed are stored in history_list_of_failed_primed_disj
 (local-concretize bdd_alpha)))
;;

;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun real-compute-mixed-alpha-image-atom-under (predicate context)
;; is suppose to be the dual of (real-compute-mixed-alpha-image-atom-over)
;; High potential of errors......
   (let* ((bdd_alpha (bdd_0)) ;; initialize alpha with false
          (primed_bdd_list_previous_fail (list (bdd_1)))  ;; initialize fail with true
          (primed_bdd_list_current_fail nil)
          ;;(unprimed_bdd_list_previous_fail (list (bdd_0)))  ;; initialize fail with false
          ;;(unprimed_bdd_list_current_fail nil)
          (history_list_of_failed_primed_disj nil)
         (generated_primed_bool_exprs (reverse ;; no reverse !
              (generate-possible-conj-bool-expr bdd_alpha 
               primed_bdd_list_previous_fail (list (bdd_1))  t)))
         ;;(generated_unprimed_bool_exprs (reverse
           ;;   (generate-possible-disj-bool-expr bdd_alpha 
             ;;    unprimed_bdd_list_previous_fail (list (bdd_0))  nil)))
	 )
 ;; test "primed disjunctions"
   (loop while (not (null generated_primed_bool_exprs)) do
       (dolist (one_bool_expr generated_primed_bool_exprs)
          (if (conj-subsumed-by-alpha 
                    one_bool_expr bdd_alpha primed_bdd_list_previous_fail)
              (setq primed_bdd_list_current_fail (cons one_bool_expr 
                                                primed_bdd_list_current_fail))
            (let ((pvs-expr (local-gamma-image one_bool_expr))
                  (context-expr (gamma-image-contex context)))
          (if  (call-decision-procedure-and-prove 
                         (mk-conjunction (list context-expr predicate)) 
                                  pvs-expr)
                (setq bdd_alpha (bdd_and bdd_alpha one_bool_expr))
                (setq primed_bdd_list_current_fail 
                            (cons one_bool_expr primed_bdd_list_current_fail))
                ))
          ))
        (setq generated_primed_bool_exprs (if (null primed_bdd_list_current_fail) nil
             (generate-possible-disj-bool-expr bdd_alpha 
                  primed_bdd_list_previous_fail   primed_bdd_list_current_fail t)))
        (setq primed_bdd_list_previous_fail primed_bdd_list_current_fail)
        (setq history_list_of_failed_primed_disj 
                (append history_list_of_failed_primed_disj primed_bdd_list_current_fail))
        (setq primed_bdd_list_current_fail nil)
      )
  ;; test "unprimed conjunctiosn => failed primed disjunction"
  ;; failed are stored in history_list_of_failed_primed_disj
 (local-concretize bdd_alpha)))
;;

;;
;;
;;

(defun only-antecedants-in-context (generated_unprimed_bool_exprs context)
 (let* ((pvs-context-expr (gamma-image-contex context))
        (pvs-bdd-context-expr pvs-context-expr)
        (new-list (remove nil (mapcar #'(lambda (one-ant) 
               (let* ((pvs-ant-expr (local-gamma-image one-ant))
                      (bdd-ant-expr pvs-ant-expr)
                      (bdd-to-check (make-implication pvs-bdd-context-expr 
                                        (negate bdd-ant-expr))))
         (if (tc-eq *true* bdd-to-check) nil
              (if 
               (call-decision-procedure-and-prove 
                    pvs-context-expr (make-negation pvs-ant-expr))
                      nil
                      one-ant))))  generated_unprimed_bool_exprs))))
  new-list))

;;(defun only-antecedants-in-context (generated_unprimed_bool_exprs context)                     generated_unprimed_bool_exprs)
   
;;    
;; test if the conjunction of the bool_expr and any element of
;; fail is in fail which means that bool_expr cannot be tested succesfully
;;

(defun subsumed-by-alpha (bool_expr bdd_fail bdd_alpha)
  (bdd-1? (bdd_implies (bdd_and bool_expr bdd_alpha) bdd_fail)))


;;
;;

(defun disj-subsumed-by-alpha (bool_expr bdd_alpha bdd_list_previous_fail)
    (member (bdd_and bdd_alpha bool_expr) bdd_list_previous_fail))

(defun conj-subsumed-by-alpha (bool_expr bdd_alpha bdd_list_previous_fail)
    (member (bdd_or bdd_alpha bool_expr) bdd_list_previous_fail))

(defun not-exclusive? (bdd excl_bdd)
  (declare (ignore bdd excl_bdd))
  ())

;;
;; why a new function for this? bool_expr can simply be
;; any bool_expr with primed and unprimed variables
;;

(defun mixed-subsumed-by-alpha (bool_expr bdd_fail bdd_alpha)
   (bdd-1?  (bdd_implies (bdd_and bool_expr bdd_alpha) bdd_fail)))

;; calls decision procedure and get the result
(defun call-decision-procedure-and-prove (pvs-hyp-formula pvs-concl-formula)
 (setq *counter-possible-prf* (+ 1 *counter-possible-prf*))
 (setq *proof-counter* (+ 1 *proof-counter*))
 (let* ((free-vars-hyp (free-state-components pvs-hyp-formula))
       (free-vars-concl (free-state-components pvs-concl-formula))
       (common-vars? (intersection free-vars-hyp free-vars-concl)))
 (if (null common-vars?) nil
 (let ((*suppress-printing* T)
       (*suppress-msg* T)
       (proof-obligation  
              (mk-formula-decl (makesym (format nil "abstraction"))
                   (typecheck (mk-implication pvs-hyp-formula pvs-concl-formula)))))
  (update-context-with-new-stuff proof-obligation)
  (setq *counter-attempt* (+ 1 *counter-attempt*))
  (prove-decl  proof-obligation  
            :strategy `(try (apply (then*
      (skosimp*) (branch (bddsimp) ((then*  (assert)))))) (fail) (fail))
    )
  (if (proved? proof-obligation )
      t nil))
)))

(defun update-context-with-new-stuff (proof-obligation)
 (setf (module proof-obligation ) *current-theory*))

;;
;;
;;;;;;;
;;
;;

(defun depends-on-only-primed? (predicate)
 (let* ((pred-vars (freevars predicate))
        (depends-on-primed (intersection (mapcar #'id *primed-variables*) (mapcar #'id pred-vars) :test 'tc-eq))
        (depends-on-unprimed (intersection (mapcar #'id *unprimed-variables*) (mapcar #'id pred-vars) :test 'tc-eq)))
   (and (not (null depends-on-primed ))
             (null depends-on-unprimed ))))

(defun depends-on-only-unprimed? (predicate)
 (let* ((pred-vars (freevars predicate))
        (depends-on-primed (intersection (mapcar #'id *primed-variables*) 
                (mapcar #'id pred-vars) :test 'tc-eq))
        (depends-on-unprimed (intersection (mapcar #'id *unprimed-variables*) 
           (mapcar #'id pred-vars) :test 'tc-eq)))
   (and      (null depends-on-primed )
        (not (null depends-on-unprimed )))))

;;;;;;;
;;
;; Utils functions
;;
;;


(defun make-list-of-indices (maxnumber)
  (make-list-from-1-to-max maxnumber 1 nil))

(defun make-list-from-1-to-max (maxnumber current list-elem)
  (if (> current maxnumber) (reverse list-elem)
       (make-list-from-1-to-max maxnumber (+ 1 current )
            (cons current list-elem))))

;;
;; define free variables of an expression involving state.
;; pc1(state) -> [pc1]
;;

(defun free-state-components (expr)
  (let ((free-comp (get-free-state-components expr)))
    free-comp))

(defun get-free-state-components (expr) 
 (if (consp expr)
            (loop for x in expr
	append (let ((result (get-free-state-components x)))
		(if (listp result)
		    result
		    (list result))))
  (let* ((*lift-if-updates* T)
	 (expr (if (expr? expr) ;; boolean? can not be used with assignment..
                    (if (and (boolean? expr)
			(equation? expr))
		   (lift-if-expr expr)
		   expr) expr )))
    (cond ((disjunction? expr) (append 
                                 (get-free-state-components (args1 expr))
                                 (get-free-state-components (args2 expr))))
          ((conjunction? expr) (append 
                                 (get-free-state-components (args1 expr))
                                 (get-free-state-components (args2 expr))))
          ((iff-or-boolean-equation? expr)  (append 
                                 (get-free-state-components (args1 expr))
                                 (get-free-state-components (args2 expr))))
          ((implication? expr)  (append 
                                 (get-free-state-components (args1 expr))
                                 (get-free-state-components (args2 expr))))
          ((negation? expr)  (get-free-state-components (args1 expr)))
          ((branch? expr)  nil)
          ((disequation? expr)  (append 
                                 (get-free-state-components (args1 expr))
                                 (get-free-state-components (args2 expr))))
          ((equation? expr)    (append 
                                 (get-free-state-components (args1 expr))
                                 (get-free-state-components (args2 expr))))
          ((forall-expr? expr) (get-free-state-components (expression expr)))
          ((exists-expr? expr) (get-free-state-components (expression expr)))
          ((lambda-expr? expr) (get-free-state-components (expression expr)))
          ((application? expr) (append 
                                 (get-free-state-components (operator expr))
                                 (get-free-state-components (arguments expr))))
          ((field-application? expr) (list (id expr))) ;; basic case
          ((update-expr?  expr)  (append 
                                 (get-free-state-components (expression expr))
                                 (get-free-state-components (assignments  expr))))
          ((or (assignment? expr) (uni-assignment? expr))
                                 (append 
                                 (mapcar #'(lambda (field) (id field))
                                     (args1 expr)) ;; basic case
                         nil ;;(get-fre...  (expression expr)) ???
                       ))
          (t nil)
       ))))
  
(defun updated-state-var (expr)
 (let ((state-var (get-updated-state-var expr)))
  (if (consp state-var) (car state-var) state-var)))

(defun get-updated-state-var (expr)
  (if (consp expr)
            (loop for x in expr
	append (let ((result (get-updated-state-var x)))
		(if (listp result)
		    result
		    (list result))))
  (let* ((*lift-if-updates* T)
	 (expr (if (expr? expr) ;; boolean? can not be used with assignment..
                    (if (and (boolean? expr)
			(equation? expr))
		   (lift-if-expr expr)
		   expr) expr )))
    (cond ((disjunction? expr) (append 
                                 (get-updated-state-var (args1 expr))
                                 (get-updated-state-var (args2 expr))))
          ((conjunction? expr) (append 
                                 (get-updated-state-var (args1 expr))
                                 (get-updated-state-var (args2 expr))))
          ((iff-or-boolean-equation? expr)  (append 
                                 (get-updated-state-var (args1 expr))
                                 (get-updated-state-var (args2 expr))))
          ((implication? expr)  (append 
                                 (get-updated-state-var (args1 expr))
                                 (get-updated-state-var (args2 expr))))
          ((negation? expr)  (get-updated-state-var (args1 expr)))
          ((branch? expr)  nil)
          ((disequation? expr)  (append 
                                 (get-updated-state-var (args1 expr))
                                 (get-updated-state-var (args2 expr))))
          ((equation? expr)    (append 
                                 (get-updated-state-var (args1 expr))
                                 (get-updated-state-var (args2 expr))))
          ((forall-expr? expr) (get-updated-state-var (expression expr)))
          ((exists-expr? expr) (get-updated-state-var (expression expr)))
          ((lambda-expr? expr) (get-updated-state-var (expression expr)))
          ((application? expr) (append 
                                 (get-updated-state-var (operator expr))
                                 (get-updated-state-var (arguments expr))))
          ((field-application? expr) nil) 
          ((update-expr?  expr) (expression expr))
          ((or (assignment? expr) (uni-assignment? expr)) nil)
          (t nil)
       ))))
  
;;
;;encode-into-bdd-pvs-expr
;; 

(defvar *zozo* nil)

(defun fresh-bdd-varid ()
 (funcall *bdd-counter*))

(defun encode-into-bdd-pvs-expr (pvs-expr)
  pvs-expr)

(defun encode-expr-with-one-bool-var (pvs-expr)
 (let*  ((bddvarid (fresh-bdd-varid))
         (bdd-variable (bdd_create_var bddvarid)))
   (setf (gethash bddvarid  *bdd-pvs-hash*) pvs-expr)
  bdd-variable))

(defun same-expression (e1 e2)
 (tc-eq (typecheck e1) (typecheck e2)))

          
(defun make-negation-with-p (expr)
 (make-negation expr))


(defun create-unchanged-expr (list-indices fml)
  (let* ((abs-state-var-1 (unparse *abs-state-var-1* :string t))
	 (abs-state-var-2 (unparse *abs-state-var-2* :string t))
	 (remaning-comp (intersection 
			 (mapcar #'string  (mapcar #'id (fields *abs-state-type*)))
			 (mapcar #'string  (mapcar #'id (fields *state-type*)))))
	 (free-comp (mapcar #'string (get-free-state-components fml)))
	 (unchange-comp (set-difference remaning-comp free-comp))
	 (unchange-comp
	  (mapcar #'(lambda (one-field)
		      (make-equality 
		       (typecheck(pc-parse (format nil "~a(~a)" 
					     one-field abs-state-var-1) 'expr))
		       (typecheck(pc-parse (format nil "~a(~a)" 
					     one-field abs-state-var-2) 'expr))
		       ))
	    unchange-comp))
	 (unchanged-bool-vars
	  (mapcar #'(lambda (one-indice) 
		      (make-equality 
		       (typecheck(pc-parse
				     (format nil "~a(~a)" 
				       (nth (1- one-indice)
					    *new-abstract-field-ids*)
				       abs-state-var-1) 'expr))
		       (typecheck(pc-parse
				     (format nil "~a(~a)" 
				       (nth (1- one-indice)
					    *new-abstract-field-ids*)
				       abs-state-var-2) 'expr))))
	    list-indices)))
    (mk-conjunction (append unchange-comp unchanged-bool-vars))))
        

(defun normalize-decompose-equality (expr)
 (if (not (or (decomposable-equality? expr)
			(and (negation? expr)
			     (decomposable-equality? (args1 expr)))))
       expr
  (let* ((fm (or (decomposable-equality? expr)
			(and (negation? expr)
			     (decomposable-equality? (args1 expr)))))
	(ffm (when fm expr))
	(fmla (when fm
		(if (negation? ffm)
		    (args1 ffm)
		    ffm)))
	(lhs (when fmla (args1 fmla)))
	(rhs (when fmla (args2 fmla)))
	(comp-equalities (when fmla
			   (component-equalities
			    lhs rhs
			    (find-declared-adt-supertype (type lhs))))))
 (if (and (variable? lhs) (variable? rhs)) expr
  (beta-reduce comp-equalities) ))))

(defun make-bdd-exclusivity (list-indices)
 (if *exclusive?* (do-make-bdd-exclusivity list-indices list-indices (bdd_1))
     (bdd_1))
  )

(defun do-make-bdd-exclusivity (list-indices acc-list-ind acc_bdd)
   (if (null acc-list-ind)  acc_bdd
     (let* ((bdd-var-i (car acc-list-ind))
            (others-i (remove bdd-var-i list-indices))
            (bdd-var (bdd_create_var bdd-var-i))
            (others-var (mapcar #'(lambda (i) (bdd_not(bdd_create_var i)))
                              others-i))
            (others-var (bdd_and_list others-var))
            (bdd-exc-expr (bdd_implies bdd-var others-var)))
      (do-make-bdd-exclusivity list-indices (cdr acc-list-ind)
                  (bdd_and bdd-exc-expr acc_bdd)))))



(defun bdd_and_list (list-bdd)
  (do_bdd_and_list list-bdd (bdd_1)))

(defun do_bdd_and_list (list-bdd acc_bdd)
 (if (null list-bdd) acc_bdd
   (do_bdd_and_list (cdr list-bdd) (bdd_and (car list-bdd) acc_bdd))))
