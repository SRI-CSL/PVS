;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pvs-mu.lisp -- Interface to the Mu-calculus model-checker
;; Author          : Sree, Shankar and Saidi
;; Created On      : Wed May  3 19:51:22 1995
;; Last Modified By: Sam Owre
;; Last Modified On: Sat May 23 23:21:47 1998
;; Update Count    : 17
;; Status          : Beta test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'pvs)  

(require :foreign)

;;;;;;;;;;;;;;;;;
;;;  Formula  ;;;
;;;;;;;;;;;;;;;;;

(ff:defforeign 'mu_mk_false_formula :entry-point "mu___mu_mk_false_formula")
;;; Formula mu_mk_false_formula (void)
(ff:defforeign 'mu_mk_true_formula :entry-point "mu___mu_mk_true_formula")
;;; Formula mu_mk_true_formula (void)
(ff:defforeign 'mu_mk_bool_var :entry-point "mu___mu_mk_bool_var")
;;; Formula mu_mk_bool_var (char *name)
(ff:defforeign 'mu_check_bool_var :entry-point "mu___mu_check_bool_var")
;;; Formula mu_check_bool_var
(ff:defforeign 'mu_check_mk_bool_var :entry-point "mu___mu_check_mk_bool_var")
;;; Formula mu_check_mk_bool_var
(ff:defforeign 'mu_mk_ite_formula :entry-point "mu___mu_mk_ite_formula")
;;; Formula mu_mk_ite_formula (Formula cond, Formula then_part, Formula else_part)
(ff:defforeign 'mu_mk_curry_application :entry-point "mu___mu_mk_curry_application")
;;; Formula mu_mk_curry_application (Term R, LIST subs, int curried)
(ff:defforeign 'mu_mk_application :entry-point "mu___mu_mk_application")
;;; Formula mu_mk_application (Term R, LIST subs, int curried)
(ff:defforeign 'mu_mk_forall :entry-point "mu___mu_mk_forall") ;; (listvars fml1) always formula
(ff:defforeign 'mu_mk_exists :entry-point "mu___mu_mk_exists") ;; (listvars fml1) always formula

(ff:defforeign 'mu_mk_implies_formula :entry-point "mu___mu_mk_implies_formula") ;; (fml1 fml2)
(ff:defforeign 'mu_mk_equiv_formula :entry-point "mu___mu_mk_equiv_formula");; (fml1 fml2)
(ff:defforeign 'mu_mk_xor_formula :entry-point "mu___mu_mk_xor_formula");; (fml1 fml2)
(ff:defforeign 'mu_mk_or_formula :entry-point "mu___mu_mk_or_formula");; (fml1 fml2)
(ff:defforeign 'mu_mk_and_formula :entry-point "mu___mu_mk_and_formula");; (fml1 fml2)
(ff:defforeign 'mu_mk_not_formula :entry-point "mu___mu_mk_not_formula");; (fml1 fml2)
(ff:defforeign 'mu_mk_cofactor :entry-point "mu___mu_mk_cofactor");; (fml1 fml2) always formula

;;;;;;;;;;;;;;;
;;;  Term   ;;;
;;;;;;;;;;;;;;;
(ff:defforeign 'mu_mk_abstraction :entry-point "mu___mu_mk_abstraction")
;;; Term mu_mk_abstraction (LIST vars, Formula f1)
(ff:defforeign 'mu_mk_l_fixed_point :entry-point "mu___mu_mk_l_fixed_point")
;;; Term mu_mk_fixed_point 
(ff:defforeign 'mu_mk_g_fixed_point :entry-point "mu___mu_mk_g_fixed_point")
;;; Term mu_mk_g_fixed_point 
(ff:defforeign 'mu_mk_reach :entry-point "mu___mu_mk_reach")
;;; Term mu_mk_reach (Term Next, Term S0, Term Inv)
(ff:defforeign 'mu_mk_rel_var_dcl :entry-point "mu___mu_mk_rel_var_dcl")
;;; Term mu_mk_rel_var_dcl (char *name) 
(ff:defforeign 'mu_mk_rel_var_ :entry-point "mu___mu_mk_rel_var_")
;;; Term  mu_mk_rel_var_ (R_Interpret Ip, char *name)
(ff:defforeign 'mu_mk_true_term :entry-point "mu___mu_mk_true_term")
;;; Term  mu_mk_true_term (void)
(ff:defforeign 'mu_mk_false_term :entry-point "mu___mu_mk_false_term")
;;; Term  mu_mk_false_term (void)
(ff:defforeign 'mu_mk_not_term :entry-point "mu___mu_mk_not_term") ;; (fml1)
(ff:defforeign 'mu_mk_and_term :entry-point "mu___mu_mk_and_term") ;; (fml1 fml2)
(ff:defforeign 'mu_mk_or_term :entry-point "mu___mu_mk_or_term") ;; (fml1 fml2)
(ff:defforeign 'mu_mk_equiv_term :entry-point "mu___mu_mk_equiv_term") ;; (fml1 fml2)
(ff:defforeign 'mu_mk_implies_term :entry-point "mu___mu_mk_implies_term") ;; (fml1 fml2)
(ff:defforeign 'mu_mk_xor_term :entry-point "mu___mu_mk_xor_term") ;; (fml1 fml2)

(ff:defforeign 'get_bdd_var_id :entry-point "mu___get_bdd_var_id") ;; (int)
(ff:defforeign 'get_mu_bool_var_name :entry-point "mu___get_mu_bool_var_name") ;; (char)
;;;;;;;;;;;;;;;;;;;
;;;  Lists      ;;;
;;;;;;;;;;;;;;;;;;;
;; 

(ff:defforeign 'append_cont :entry-point "mu___append_cont")
(ff:defforeign 'empty_list :entry-point "mu___empty_list")

;;;
;;; Flags

(ff:defforeign 'set_mu_bdd_ordering :entry-point "mu___set_mu_bdd_ordering")
(ff:defforeign 'set_mu_warnings :entry-point "mu___set_mu_warnings")
(ff:defforeign 'set_mu_simplify_frontier :entry-point "mu___set_mu_simplify_frontier")
(ff:defforeign 'set_mu_verbose :entry-point "mu___set_mu_verbose")
(ff:defforeign 'set_mu_bdd_use_neg_edges :entry-point "mu___set_mu_bdd_use_neg_edges")

;;
;;
;; GC management: not needed, "modelcheck_formula" takes care of it.
;;

;;;;;;;;;;;;;;;;;;;
;;;  print      ;;;
;;;;;;;;;;;;;;;;;;;

(ff:defforeign 'pvs_mu_print_formula :entry-point "mu___pvs_mu_print_formula")
(ff:defforeign 'pvs_mu_print_term :entry-point "mu___pvs_mu_print_term")

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main function   ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(ff:defforeign 'mu_init :entry-point "mu___mu_init")
(ff:defforeign 'mu_quit :entry-point "mu___mu_quit")
(ff:defforeign 'modelcheck_formula :entry-point "mu___modelcheck_formula")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global variables     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *build-mu-term* nil) ;; t when converting a term and nil when converting a formula
(defvar *build-rel-var* nil) ;; t when mu_mk_rel_var and nil when mu_mk_bool_var
(defvar *build-arguments* nil)
(defvar *build-access-var* nil) ;; t when mu_check_bool_var and nil otherwise.

(defvar *expand-term* nil) ;; t when a term should be fully expanded

(defvar *convert-list* nil)
(defvar *list-of-relational-vars* nil) ;; list of expressions
(defvar *incl-excl-var-id-indx-pairs*  nil) ;; (bddvarid . index)


(addrule 'musimp () ((fnums *) dynamic-ordering? irredundant?)
  (musimp-fun fnums dynamic-ordering? irredundant?)
  "MU Calculus Simplification.  Dynamic-ordering? set to T means the BDD
package can reorder literals to reduce BDD size.  Irredundant? T computes
the disjunctive normal form of the result (which can take quite a bit of
time)."
  "~%Applying musimp,")


(defun musimp-fun (&optional fnums dynamic-ordering? irredundant?)
  #'(lambda (ps) (run-musimp ps fnums dynamic-ordering? irredundant?)))


(defun run-musimp (ps fnums dynamic-ordering? irredundant?)
  (bdd_init)
  (mu_init)
  (let* ((init-real-time (get-internal-real-time))
	 (init-run-time (get-run-time))
	 (*bdd-initialized* t)
	 (*pvs-bdd-hash* (make-hash-table
			  :hash-function 'pvs-sxhash :test 'tc-eq))
	 (*bdd-pvs-hash* (make-hash-table))
	 (*pvs-bdd-inclusivity-formulas* nil)
	 (*bdd-counter* *bdd-counter*)
	 (*recognizer-forms-alist* nil)
         (*mu-subtype-list* nil)
         (*incl-excl-var-id-indx-pairs* nil)
	 (sforms (s-forms (current-goal ps)))
	 (selected-sforms (select-seq sforms fnums))
	 (remaining-sforms (delete-seq sforms fnums))
	 (mu-formula
	  (make-mu-restriction
	   (convert-pvs-to-mu
	    (make-conjunction
	     (mapcar #'(lambda (sf) (negate (formula sf)))
	       selected-sforms)))
	   (make-mu-conjunction
	    (loop for x in  *pvs-bdd-inclusivity-formulas*
		  when (null (freevars (car x)));;NSH(8.17.95): subtypes
		  collect
		  (cdr x)))))
	 (mu-output (run-pvsmu mu-formula dynamic-ordering?))
	 (sum-of-cubes (bdd_sum_of_cubes mu-output (if irredundant? 1 0)))
	 (list-of-conjuncts (mu-translate-from-bdd-list sum-of-cubes))
         (lit-list (mu-from-bdd-list-to-pvs-list list-of-conjuncts)))
    (mu_quit)
    (bdd_quit)
    (cond ((zerop bdd_interrupted)
	   (multiple-value-prog1
	    (mu-add-bdd-subgoals ps sforms lit-list remaining-sforms)
	    (format t
		"~%MU simplification took ~,2,-3f real, ~,2,-3f cpu seconds"
	      (realtime-since init-real-time) (runtime-since init-run-time))))
	  (t (format t "~%MU Simplifier interrupted")
	     (values 'X nil)))))

(defun mu-from-bdd-list-to-pvs-list (list-of-conjuncts)
   (init-hash-tables) ;; definition in mu.lisp
  (let* ((lit-list (mapcar #'(lambda (conj)
                            (let ((list-conj
			     (mapcar #'(lambda (lit)
                                   (let* ((is-list (consp lit))
                                          (hash-expression 
				            (if is-list
					     (gethash (car lit) *bdd-pvs-hash*)
                                             (gethash lit *bdd-pvs-hash*)))
                                          (can-not-decode 
                                             (null hash-expression))
                                          (pvs-expression
                                             (if can-not-decode nil
                                              (if is-list hash-expression 
                                               (make-negation 
                                                  hash-expression )))))
                                    pvs-expression ))
		      	 conj)))
                     (if (member nil list-conj) nil list-conj)))
		   list-of-conjuncts)))
  (assert (hash-table-p *pvs-bdd-hash*))
    lit-list)
)


(defun mu-add-bdd-subgoals (ps sforms lit-list remaining-sforms)
 (let ((can-decode (not (member nil lit-list))))
   (if can-decode 
            (add-bdd-subgoals ps sforms lit-list remaining-sforms) 
           (progn 	
           (format t "~%Failed to Model check:
    could not decode binary encodings of scalars.")
            (values 'X nil nil))))
)


(defun init-hash-tables ()
 (setf (gethash *true* *bdd-pvs-hash*) *true*)
)


;; run-pvsmu

(defun run-pvsmu (mu-formula dynamic-ordering?)
  (if dynamic-ordering?
      (set_bdd_do_dynamic_ordering 1)
      (set_bdd_do_dynamic_ordering 0))
  (modelcheck_formula mu-formula))

;; convert-pvs-to-mu-formula
;; calls convert-pvs-to-mu-formula*

(defun convert-pvs-to-mu (expr);;expr must be of boolean type
  ;; Should we lift-if here rather than in convert-pvs-to-mu*?
  (let ((*bound-variables* nil)
	(*mu-nu-lambda-bindings-list* nil)
        (*list-of-relational-vars* nil)
	(mu-expr (convert-pvs-to-mu-formula expr)));; Formulas come first
    mu-expr))


;; Hassen.
;;  a mu-formula has the following structure:
;;  Formula ::=   Var
;;              | not Formula
;;              | forall(List_vars) .Formula
;;              | exists(List_vars) .Formula
;;              | Formula {or,and,=>,<=>,cof} Formula           
;;              | Term (List_Formula) ;; application
;;  Term ::=   Var
;;           | not term
;;           | Term {or,and,=>,<=>,cof} Term                
;;           | Lambda (List_Vars). Formula
;;           | MU/NU (list_vars). Term
;;


;; convert-pvs-to-mu* is the real workhorse.
;; [P conn Q] ==> [P] conn [Q]
;; [NOT P] ==> NOT [P]
;; [IF P THEN Q ELSE R] ==> ([P]?[Q]:[R])
;; [a/=b] ==> NOT [a = b]
;; [(if A THEN b ELSE c) = d] ==> [if A THEN b = d ELSE c = d]
;; [d  = (if A THEN b ELSE c)] ==> [if A THEN d = b ELSE d = c]
;; a scalar constant, [a = b] ==> [a?(b)]
;;                    [b = a] ==> [a?(b)]
;; a, b of scalar type [a = b] ==> a1 = b1 and ...and an=bn
;; a, b of record type [a = b] ==> [l1(a)=l1(b)] and...and [ln(a)=ln(b)]
;; a of type [A->B], A is subrange[lo,hi], B is mu-translateable?, then
;;   [a = b] ==> [a(lo) = b(lo)] and ... and [a(hi) = b(hi)]
;;


(defun convert-pvs-to-mu-formula (expr)
  ;; building a Formula
  (let* ((*build-mu-term* nil)
	 (*expand-term* nil)
	 (fml (convert-pvs-to-mu* expr)))
    fml))


(defun convert-pvs-to-mu-term (expr)
  ;; building a Term
  (let* ((*build-mu-term* t)
	 (*expand-term* nil)
	 (trm (convert-pvs-to-mu* expr)))
    trm))

(defun convert-pvs-to-mu-term-expanded (expr)
  ;; building a Term
  (let* ((*build-mu-term* t)
         (*expand-term* t)
	 (trm (convert-pvs-to-mu* expr)))
    trm))

(defvar *mu-nu-lambda-bindings-list* nil
  "lookup list containing vars that are bound by lambda operated on mu/nu")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert PVS to MU     ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod convert-pvs-to-mu* :around ((expr expr))
  (let ((type (type expr)))
    (when (and (subtype? type)
	       (not (member expr *mu-subtype-list* :test #'tc-eq))
	       (not (assoc expr *pvs-bdd-inclusivity-formulas*
			   :test #'tc-eq)))
      (let ((constraints (collect-type-constraints expr))
	    (*mu-subtype-list* (cons expr *mu-subtype-list*)))
	(loop for x in constraints do
	      (push (cons expr (convert-pvs-to-mu* x))
		    *pvs-bdd-inclusivity-formulas*))
	(format t "~%Added constraints: ~a" constraints)))
    (let ((trm (call-next-method)))
;       (unless (consp trm)
; 	(if *build-mu-term*
; 	    (pvs_mu_print_term trm)
; 	    (pvs_mu_print_formula trm)))
      trm)))

(defmethod convert-pvs-to-mu* ((expr list))
  (loop for x in expr
	append (let ((result (convert-pvs-to-mu* x)))
		 (if (listp result)
		     result
		     (list result)))))


(defmethod convert-pvs-to-mu* ((expr cases-expr))
  (convert-pvs-to-mu* (translate-cases-to-if expr))
)

;; Shankar
(defmethod translateable-binding-expr? ((expr lambda-expr))
  (every #'(lambda (x)(mu-translateable?  (type x)))
		     (bindings expr)))

(defmethod translateable-binding-expr? ((expr binding-expr))
  (every #'(lambda (x)(strict-mu-translateable?  (type x)))
		     (bindings expr)))
;;

(defmethod convert-pvs-to-mu* ((expr expr));;NSH(8.17.95) added find-supertype
  (cond ((tc-eq expr *true*) (mu-mk-true))
	((tc-eq expr *false*) (mu-mk-false))
	;;((reachable? expr) (reach_pf))
	((and (binding-expr? expr);;NSH(6.19.98):remove find-supertype
              (translateable-binding-expr? expr))
	 (convert-pvs-to-mu-binding-expr expr))
        ((scalar-constant? expr) (make-scalar-constant-bits expr))
        ((scalar?  expr)  (uncurry-application-and-mu-convert expr))
	((recordtype? (find-supertype (type expr)));;(break)
	 (convert-pvs-to-mu*
	  (loop for decl in (fields (find-supertype (type expr)))
		collect (beta-reduce
			 (make!-field-application
			  (id decl) expr))))) 
	((and (funtype? (find-supertype (type expr)))
	      (mu-translateable? (find-supertype (type expr))))
	 (let* ((range (sub-range? (domain (type expr))))
		(lo (car range))
		(hi (cdr range))
		(args-list
		 (loop for i from lo to hi
		       collect (beta-reduce
				(make-application
				    expr
				  (make-number-expr i))))))
	   (convert-pvs-to-mu* args-list)))
	(t (make-mu-variable expr))))


(defmethod convert-pvs-to-mu* ((expr application))
  (let* ((*lift-if-updates* T)
	 (expr (if (boolean-equation? expr)
		   (lift-if-expr expr)
		   expr)))
    (cond ((disjunction? expr) (convert-pvs-to-mu-disjunction expr))
          ((conjunction? expr) (convert-pvs-to-mu-conjunction expr))  
          ((iff-or-boolean-equation? expr)  (convert-pvs-to-mu-iff expr))
          ((implication? expr)  (convert-pvs-to-mu-implication expr))
          ((negation? expr)   (convert-pvs-to-mu-negation expr))
          ((branch? expr)  (convert-pvs-to-mu-branch  expr))
          ((disequation? expr)  (convert-pvs-to-mu-inequality  expr))
          ((equation? expr) (convert-pvs-to-mu-equality expr))
          ((mu-nu-expr-application? expr) (convert-pvs-to-mu-nu-application  expr))
          ((mu-nu-expr? expr) (convert-pvs-to-mu-nu-expression expr))
          ((reachable-expr? expr) (convert-pvs-to-mu-reachable expr))
          ((not (tc-eq (find-supertype (type expr)) *boolean*))
            (call-next-method))
          (t;; Other application like R(x,y)
            (uncurry-application-and-mu-convert expr))
)))



(defmethod convert-pvs-to-mu* ((expr number-expr))
  (convert-number (number expr)))



(defun convert-pvs-to-mu-binding-expr (expr)
  (let* ((expr-bindings (bindings expr))
	 (variables-bindings
	  (mapcar #'make-variable-expr expr-bindings))
	 (old-inclusivities *pvs-bdd-inclusivity-formulas*)
	 (boundvars (let ((*build-access-var* t)
			  (*build-mu-term* nil));; Hassen 11/12/98 : to avoid call mu_mk_rel_var
		      (lisp-to-c-list
                       (convert-pvs-to-mu* variables-bindings))))
	 (*bound-variables* (append variables-bindings *bound-variables*))
	 (new-inclusivities (ldiff *pvs-bdd-inclusivity-formulas*
				   old-inclusivities))
	 (boundexpr (convert-pvs-to-mu-formula (expression expr))))
    (setq *pvs-bdd-inclusivity-formulas*;;NSH(8.17.95): to collect
	  ;;subtypes from inside binding expr.
	  (set-difference *pvs-bdd-inclusivity-formulas*
			  new-inclusivities
			  :key #'car
			  :test #'tc-eq))
    (cond ((forall-expr? expr) 
	   (convert-pvs-to-mu-forall  boundvars boundexpr new-inclusivities))
	  ((exists-expr? expr) 
	   (convert-pvs-to-mu-exists  boundvars boundexpr new-inclusivities))
	  ((lambda-expr? expr)
	   (convert-pvs-to-mu-lambda  boundvars boundexpr)))))




(defun convert-pvs-to-mu-forall (boundvars boundexpr new-inclusivities)
  (if new-inclusivities
         (mu_mk_forall  boundvars
                 (mu-mk-implies (make-mu-conjunction 
                      (mapcar #'cdr new-inclusivities)) boundexpr))
         (mu_mk_forall  boundvars boundexpr)
  ))


(defun convert-pvs-to-mu-exists (boundvars boundexpr new-inclusivities)
  (if new-inclusivities
         (mu_mk_exists boundvars
                (mu-mk-and 
                     (make-mu-conjunction  (mapcar #'cdr new-inclusivities)) boundexpr)) 
         (mu_mk_exists  boundvars boundexpr) 
  ))


(defun convert-pvs-to-mu-lambda (boundvars boundexpr )
   (mu_mk_abstraction boundvars boundexpr)
)


(defun convert-pvs-to-mu-disjunction (expr)
 (let ((fml1 (convert-pvs-to-mu* (args1 expr)))
       (fml2 (convert-pvs-to-mu* (args2 expr))))
 (mu-mk-or fml1 fml2))
)

(defun convert-pvs-to-mu-conjunction (expr) 
 (let ((fml1 (convert-pvs-to-mu* (args1 expr)))
       (fml2 (convert-pvs-to-mu* (args2 expr))))
 (mu-mk-and  fml1 fml2))
)



(defun convert-pvs-to-mu-iff (expr)
 (let ((fml1 (convert-pvs-to-mu* (args1 expr)))
       (fml2 (convert-pvs-to-mu* (args2 expr))))
 (mu-mk-equiv fml1 fml2))
)

(defun convert-pvs-to-mu-implication (expr)
 (let ((fml1 (convert-pvs-to-mu* (args1 expr)))
       (fml2 (convert-pvs-to-mu* (args2 expr))))
 (mu-mk-implies fml1 fml2))
)


(defun convert-pvs-to-mu-negation (expr)
 (let ((fml1 (convert-pvs-to-mu* (args1 expr))))
    (mu-mk-not fml1))
)



(defun convert-pvs-to-mu-branch  (expr)
 (let ((result (assert-test0 (condition expr))))
	(cond ((eq result 'TRUE)
	       (convert-pvs-to-mu* (then-part expr)))
	      ((eq result 'FALSE)
	       (convert-pvs-to-mu* (else-part expr)))
	      (t 
	       (let ((fml1 (convert-pvs-to-mu* (condition expr)))
		     (fml2 (convert-pvs-to-mu* (then-part expr)))
		     (fml3 (convert-pvs-to-mu* (else-part expr))))
                  (mu_mk_ite_formula fml1 fml2 fml3)
             ))))
)


(defun convert-pvs-to-mu-inequality  (expr)
 (let ((fml1 (convert-pvs-to-mu* (make-equality (args1 expr)(args2 expr)))))
                 (mu-mk-not fml1))
)



(defun convert-pvs-to-mu-nu-application  (expr)
      (let* ((*expand-term* nil)
             (exprargs (arguments expr))
	     (argsfml (let ((*build-arguments* t))
                     (lisp-to-c-list (convert-pvs-to-mu* exprargs))))
	     (muop (operator expr))
	     (mu-or-nu (string (id (operator muop))))
	     (muargs1bindgs (bindings (args1 muop)))
	     (*mu-nu-lambda-bindings-list*
	      (if muargs1bindgs
		  (append muargs1bindgs
			  *mu-nu-lambda-bindings-list*) 
		  (pvs-error "Can not model-check" 
		    (format nil "Expression ~a ~% of type ~a should be expanded"
		      (unparse (args1 muop) :string t) 
		      (unparse (type (args1 muop)) :string t)))))
	     (muarg1fml (let ((*build-rel-var* t)) 
                                 (car (convert-pvs-to-mu*
			 (mapcar #'make-variable-expr muargs1bindgs)))))
	     (muarg2expr (expression (args1 muop)))
	     (muarg2fml (convert-pvs-to-mu-term-expanded muarg2expr))
	     (muexprfml
               (make-mu-application
                      (if (string= mu-or-nu "mu") 
                          (mu_mk_l_fixed_point muarg1fml muarg2fml)
                          (mu_mk_g_fixed_point muarg1fml muarg2fml))  argsfml))
          )
	muexprfml )
)


(defun convert-pvs-to-mu-nu-expression (expr)
  (let* ((*expand-term* nil)
	 (muop (operator expr))
	 (mu-or-nu (string (id muop)))
	 (muargs1bindgs (bindings (args1 expr)));;args1muop islambda-expr
	 (muarg1fml 
	  (let ((*build-rel-var* t)) 
	    (car (convert-pvs-to-mu*
		  (mapcar #'make-variable-expr muargs1bindgs)))))
	 (*mu-nu-lambda-bindings-list*;;NSH(6.23.95)
	  (if muargs1bindgs
	      (append muargs1bindgs
		      *mu-nu-lambda-bindings-list*) 
	      (pvs-error "Can not model-check" 
		(format nil "Expression ~a ~% of type ~a should be expanded"
		  (unparse (args1 muop) :string t) 
		  (unparse (type (args1 muop)) :string t)))))
	 (muarg2expr (expression (args1 expr)))
	 (muarg2fml (convert-pvs-to-mu-term-expanded muarg2expr))
	 (muexprstr
	  (if (string= mu-or-nu "mu") 
	      (mu_mk_l_fixed_point muarg1fml muarg2fml)
	      (mu_mk_g_fixed_point muarg1fml muarg2fml))))
    muexprstr))

;;HS 03/05/99
(defun convert-pvs-to-mu-reachable (expr)
  (let* ((mu-list-args (let ((*build-arguments* t))
			 (convert-pvs-to-mu-formula (arguments expr))))
         (reach-list-args (convert-pvs-to-mu-term-expanded
			   (arguments (operator expr))))
         (nbofargs (length (arguments (operator expr))))
         (mu-reach-expr
	  (cond ((equal 2 nbofargs)
		 (mu_mk_reach (nth 0 reach-list-args) 
			      (nth 1 reach-list-args)
			      (mu_mk_true_term) ))
		((equal 3 nbofargs)
		 (mu_mk_reach (nth 0 reach-list-args) 
			      (nth 1 reach-list-args)
			      (nth 2 reach-list-args))))))
    (mu_mk_application mu-reach-expr (lisp-to-c-list mu-list-args) 0)))

(defun convert-pvs-to-mu-equality (expr)
  (let ((expr (beta-reduce expr)))
    (cond
     ((and (funtype? (find-supertype (type (args1 expr))))
	   (mu-translateable? (type (args1 expr))))
      (convert-pvs-to-mu-equality-with-funtype-in-arg expr)) 
     ((or (branch? (args1 expr));;NSH(2.2.96): added cases?
	  (cases-expr? (args1 expr)));;to this and next case, else      
      (convert-pvs-to-mu-equality-with-case-args expr nil))
     ((or (branch? (args2 expr))
	  (cases-expr? (args2 expr)))
      (convert-pvs-to-mu-equality-with-case-args expr t))
     ((or (scalar-constant? (args1 expr))
	  (scalar-constant? (args2 expr)))           
      (convert-pvs-to-mu-equality-with-scalarconstant-in-args expr)) 
     ((scalar? (args1 expr))
      (convert-pvs-to-mu-equality-with-scalar-arg expr)) 
     ((recordtype? (find-supertype (type (args1 expr))))
      (convert-pvs-to-mu-equality-with-recordtype-in-arg expr)) 
     ((and (funtype? (find-supertype (type (args1 expr))))
	   (mu-translateable? (find-supertype (type (args1 expr)))))
      (convert-pvs-to-mu-equality-with-funtype-mu-trans-supertype-in-arg expr)) 
     ((sub-range? (type (args1 expr)))
      (convert-pvs-to-mu-equality-with-subrangetype-in-arg expr))
     (t (make-mu-variable expr)))))


(defmethod convert-number (number)
  (let* ((len (max 1 (ceiling (log (1+ number) 2))))
         (format-bitstring (format nil "~~~d,'0b" len)) 
         (bit-string (format nil format-bitstring number))
         (lisp-list-values (nreverse  ;lsb first: little-endian
               (loop for i from 0 to (1- len)
	             collect (if (eql (digit-char-p (elt bit-string i)) 1)
		                   (mu-mk-true) (mu-mk-false))))))
 (lisp-to-c-list lisp-list-values))
)


;; given a pvs expr, it forms a basic expr-string that's bdd-isable and
;; a correspondng *recognizer-forms-(a? Hassen)list*: which is an assoc list such as:
;; ((x!1 (R . BVAR$1) (B . BVAR$2) (G . BVAR$3)) (y!1 (...)))
;; Then form excl/inclusives-strings:
;; enum-exclusives-string = ((NOT BVAR$1 + NOT (BVAR$3+BVAR$2)) & (NOT BVAR$2 +
;; NOT BVAR$3) & (corresponding to y!1))
;; enum-inclusives-string = find constructors of adt supertype of x!1,y!1
;;                          check if allof them occurr in recognizer-list and
;;                          then make a proper string if necessary.
;; then, the final bdd fmla to be sent is (expr-string |(exclusives&inclusives))
;; Boundary test cases considered: when there are no enums and when the
;;                                 enum type is a singleton - in which case
;;                                 enum-exclusives = nil, but 
;;                                 enum-inclusives /= nil
;; Hassen 05/06/1998
;; reachable-expr? should be of the form Reachable(list-args1)(list-args2)
;; where list-args1 is of length 2 or 3.
;;
;;asserts len-bit bitvect bvar to be less than or equal to number num
;;this is added as a restriction to the BDD.


;;NSH(2.1.96); added reverse
;; Klaus noticed this unsoundness since the lsb was being set to the
;;msb of the position bit string.  

(defun make-mu-variable (expr);; new-make-variable
  (when (and *expand-term*
	     (funtype? (find-supertype (type expr))))
    ;; case where a variable is created for an expression that 
    ;; is expected to be expanded
    (pvs-error "Can not model-check" 
      (format nil "Expession ~a should be expanded"
	(unparse expr :string t))))
  (let ((bddname (gethash expr *pvs-bdd-hash*)))
    (if bddname
	(if (consp bddname)
	    (let ((new-bddname (cadr bddname)))
	      (cond (*build-arguments*
		     (make-argument-vars-scalar new-bddname))
		    (*build-access-var*
		     (make-binding-vars-scalar new-bddname))
		    (t new-bddname)))
	    (mu-create-bool-var (format nil "b~d" bddname)))
	(cond ((sub-range? (type expr))
	       (make-subrange-names expr))
	      ((scalar? expr)
	       (make-scalar-names expr))
	      ((recognizer-application? expr)
	       (make-mu-variable-recognizer-application expr))
	      (t (let* ((bddhash-name (make-bdd-var-id))
			(mu-expression (mu-create-bool-var bddhash-name))
			(is-rel-var (and *build-rel-var*
					 (not *build-access-var*))))
		   (setf (gethash expr *pvs-bdd-hash*) bddhash-name)
		   (unless (freevars expr)
		     (setf (gethash bddhash-name *bdd-pvs-hash*) expr))
		   (when is-rel-var
		     (push expr *list-of-relational-vars*))
		   mu-expression))))))


;; negates a particular element in a list of boolean lits (symbols)
;; and forms a conjunction of the elements

(defun make-negate-pos-conjunction (pos litlist) ;; litlist is a list of varid
                                                 ;; the created list is a C list????
  (let ((mu-formula-list
	 (loop for varid in litlist as i from 0
	       collect
	       (if (eq i pos)
		   (mu-mk-not (mu-create-bool-var varid))
		   (mu-create-bool-var varid) ))))
    (make-mu-conjunction mu-formula-list))
)
		    


(defun make-mu-variable* (list &optional accum)
  (cond ((null list)
	 (nreverse accum))
	(t (let ((answer (make-mu-variable (car list))))
	     (make-mu-variable* (cdr list)
				(if (consp answer)
				    (append (reverse answer) accum)
				    (cons answer accum)))))))
    
    
(defun uncurry-application-and-mu-convert (expr)
  (let* ((*lift-if-updates* T)
	 (old-expr expr)
	 (expr (lift-if-expr expr)))
    (if (eq expr old-expr)
	(if (recognizer-application? expr) 
	    (make-mu-variable expr)
	    (if (application? expr)
	       	(let ((op* (get-op expr));;NSH(4.5.95) extensions to handle
		      ;;more complex expressions.
		      (args* (get-args expr)))
		  (if (and (every #'(lambda (x) (mu-translateable? (type x)))
				  args*)
			   (or (lambda-expr? op*)
			       (and (variable? op*)
				    (member op* *mu-nu-lambda-bindings-list*
					    :test #'same-declaration))))
		      (let ((op-str (convert-pvs-to-mu-term op*)) ;; is a term?
			    (args-list (let ((*build-arguments* t))
                                (lisp-to-c-list (convert-pvs-to-mu* args*))))) 
                        (make-mu-application op-str args-list))            
		      (if (and (mu-translateable? (type (operator expr)))
			       (not (number-expr? (argument expr))))
			  (let* ((args-list (convert-pvs-to-mu* (argument expr)))
				 (args-list (if (listp args-list) args-list
					      (list args-list))))
			   (decode-array (operator expr) (nreverse args-list)))
                             ;; (make-mu-variable expr)) ;; Hassen 05/07/98
			  (make-mu-variable expr))))
		(make-mu-variable expr)))
	(convert-pvs-to-mu* expr))))

(defun decode-array (op args)
  (let* ((bindexpr (decode-array-format args))
	 (oplist
          (nreverse ;;NSH(6.19.98)
	   (loop for i from 0 to (1- (expt 2 (length args)))
		collect
		(convert-pvs-to-mu*
		 (make-application op
		   (make-number-expr i))))))
	 (len (if (consp oplist)
		  (if (consp (car oplist))
		      (length (car oplist))
		      1)
		  0))) ;; could not be 0! Hassen
     (if (equal 0 len) bindexpr
           (mu_mk_curry_application  
                  (mu_mk_abstraction () bindexpr)  
                        (lisp-to-c-list oplist)))
))


					  
(defun decode-array-format (args)
  (if (consp args)
      (mu_mk_ite_formula (car args) 
                   (decode-array-format (cdr args)) 
                   (decode-array-format (cdr args)))
      (mu-create-bool-var  (funcall *bdd-counter*))))

;;

(defun convert-pvs-to-mu-equality-with-funtype-in-arg (expr) 
      (let* ((dtype (domain (find-supertype (type (args1 expr)))))
	     (x (sub-range? dtype))
	     (lo (car x))
	     (hi (cdr x))
	     (arg1 (args1 expr))
	     (arg2 (args2 expr)))
      (make-mu-conjunction
       (loop for i from lo to hi
	     collect (convert-pvs-to-mu*
		      (make-equality
		       (make-application arg1 (make-number-expr i))
		       (make-application arg2 (make-number-expr i))))))))
	   


(defun convert-pvs-to-mu-equality-with-scalarconstant-in-args (expr)
      (convert-pvs-to-mu*
       (if (scalar-constant? (args1 expr))
	   (make-application
	       (recognizer (args1 expr))
	     (args2 expr))
	   (make-application
	       (recognizer (args2 expr))
	     (args1 expr)))))


(defun convert-pvs-to-mu-equality-with-scalar-arg (expr) 
  (let* ((exprarg1 (args1 expr))
	 (exprarg2 (args2 expr))
	 (scalar1 (if (bind-decl? exprarg1)
		      (make-variable-expr exprarg1)
		      exprarg1))
	 (scalar2 (if (bind-decl? exprarg2)
		      (make-variable-expr exprarg2)
		      exprarg2))
	 (muvarlist1 (make-argument-vars-scalar (make-scalar-names scalar1)))
	 (muvarlist2 (make-argument-vars-scalar (make-scalar-names scalar2))))
    (make-mu-conjunction
     (mapcar #'(lambda (x y) (mu-mk-equiv x y))
       muvarlist1 muvarlist2))))



(defun convert-pvs-to-mu-equality-with-recordtype-in-arg (expr) 
  (let* ((type (find-supertype (type (args1 expr))))
	 (fields (fields type))
	 (args1-list
	  (loop for decl in fields
		collect (beta-reduce
			 (make!-field-application
			  (id decl) (args1 expr)))))
	 (args2-list
	  (loop for decl in fields
		collect (beta-reduce
			 (make!-field-application
			  (id decl) (args2 expr)))))
	 (equality-bdds
	  (loop for x in args1-list
		as y in args2-list
		collect
		(convert-pvs-to-mu*
		 (make!-equation x y)))))
    (make-mu-conjunction equality-bdds)))


(defun convert-pvs-to-mu-equality-with-funtype-mu-trans-supertype-in-arg (expr)
  (let* (;;(type (find-supertype (type (args1 expr))))
	 ;;(range (sub-range? (domain type)))
	 ;;(lo (car range))
	 ;;(hi (cdr range))
	 (args1-list (convert-pvs-to-mu* (args1 expr)))
	 (args2-list (convert-pvs-to-mu* (args2 expr)))
	 (equality-bdds
	  (loop for x in args1-list
		as y in args2-list
		collect (mu-mk-equiv x y))))
    (make-mu-conjunction equality-bdds)))

(defun convert-pvs-to-mu-equality-with-subrangetype-in-arg (expr) 
      (let* ((args1-atoms (convert-pvs-to-mu* (args1 expr)))
	     (args2-atoms (convert-pvs-to-mu* (args2 expr)))
	     (equalities (loop for x in args1-atoms
			       as y in args2-atoms
			       collect
			  (mu-mk-equiv x y)      
                    )))
	(make-mu-conjunction equalities)))


(defun convert-pvs-to-mu-equality-with-case-args (expr rhs?)
  ;; expr is an equation.
  ;; If rhs? is nil, the lhs is a cases or branch, else the rhs is.
  (let* ((arg (if rhs? (args2 expr) (args1 expr)))
	 (arg1 (if (cases-expr? arg)
		   (translate-cases-to-if arg)
		   arg)))
    (convert-pvs-to-mu*
     (if rhs?
	 (make!-if-expr (condition arg1)
			(make!-equation (args1 expr) (then-part arg1))
			(make!-equation (args1 expr) (else-part arg1)))
	 (make!-if-expr (condition arg1)
			(make!-equation (then-part arg1) (args2 expr))
			(make!-equation (else-part arg1) (args2 expr)))))))


;;
;;
;;

(defun make-incl-excl-for-encoded-scalar (scalarvar)
   (if (scalar? scalarvar)
            (make-incl-excl-for-scalar scalarvar)
      (if (recordtype? (type scalarvar))  
              (make-incl-excl-for-recordtype-scalar  scalarvar)
            nil)
    )
)


(defun make-incl-excl-for-scalar (scalarvar)
 (let* ((recogzrs (recognizers (type scalarvar)))
	     (scalarvar (if (bind-decl? scalarvar)
			    (make-variable-expr scalarvar)
			   scalarvar))
	     (recapps (mapcar #'(lambda (x) (make-application x scalarvar))
			      recogzrs))
	     
	     (bddvarid0 (gethash scalarvar *pvs-bdd-hash*))
	     (scalar-encoding-list  ;; list of string? or list of mu_var??
	      (if (null bddvarid0)
		  (let ((newname (make-bdd-var-id)))
		    (cadr
		     (setf (gethash scalarvar *pvs-bdd-hash*)
			   (list newname
				 (loop for i from 0 to
				       (- (length recogzrs) 1)
				       collect
                                       (make-bdd-incl-excl-var-id newname i)
                                                )))))
		  (cadr bddvarid0)))
	     (encodings-list
	      (mapcar #'(lambda(x)
			  (enter-encoding-into-recognizer-form-alist x
								     scalar-encoding-list))
		      recapps)))
	(make-mu-disjunction encodings-list))
)


(defun make-incl-excl-for-recordtype-scalar (scalarvar)
	  (let* ((expr (if (bind-decl? scalarvar)
			   (make-variable-expr scalarvar)
			   scalarvar))
		 (fieldids (mapcar #'id (fields (type expr))))
		 (fieldapps (mapcar #'(lambda (x)
					(make!-field-application x expr))
				    fieldids))
		 (fieldencodings
		  (loop for entry in fieldapps
			for entryencoding =
			(make-incl-excl-for-encoded-scalar entry)
			when entryencoding collect entryencoding)))
	    (make-mu-conjunction fieldencodings))
)


(defun enter-encoding-into-recognizer-form-alist (expr
						  scalar-encoding-list)
  (let ((recexpr (recognizer-application? expr)))
    (when (not (null recexpr))
      (let* ((op (operator recexpr))
	     (arg (args1 recexpr))
	     (recogzrs (recognizers (type (args1 recexpr))))
	     (pos (position op recogzrs :test #'tc-eq))
	     (name (make-negate-pos-conjunction pos scalar-encoding-list))
	     (entry (assoc  arg *recognizer-forms-alist*
			    :test #'tc-eq)))
	(cond ((null entry)
	       (push (cons arg (list (cons op name)))
		     *recognizer-forms-alist*)
	       name
	       )
	      (t (pushnew (cons op name)
			  (cdr entry)
			  :test #'(lambda(x y) (tc-eq (car x) (car y))))
		 name
		 )))))
  )


(defun make-scalar-inclusive-formula (bvar num index)
  (if (zerop index)
      (if (logbitp 0 num)
          (mu-mk-true)
          (mu-mk-not (mu-create-bool-var
		      (make-bdd-incl-excl-var-id bvar 0))))
      (let ((bvarbit (mu-create-bool-var
		      (make-bdd-incl-excl-var-id bvar index)))
	    (bit (logbitp index num))
	    (rest-fmla (make-scalar-inclusive-formula bvar num (1- index))))
	(if bit
            (mu-mk-or (mu-mk-not bvarbit) rest-fmla)
            (mu-mk-and (mu-mk-not bvarbit) rest-fmla)))))

(defun make-subrange-inclusive-formula (bddvarid-list lo hi)
  (let* ((lo-rep (convert-number lo))
	 (hi-rep (convert-number hi))
	 (lower-rep (make-geq-bdd bddvarid-list lo-rep))
	 (higher-rep (make-leq-bdd bddvarid-list hi-rep)))
    (make-mu-conjunction (list lower-rep higher-rep))))
	


(defun make-subrange-names (expr &optional type)
  (let* ((type (if type type (type expr)))
	 (range (sub-range? type))
	 (lo (car range))
	 (hi (cdr range))
	 (len (max 1 (ceiling (log (1+ hi) 2))))
	 (bddvarid-hash
	  (gethash expr *pvs-bdd-hash*))
	 (bddvarid
	  (if bddvarid-hash
	      (car bddvarid-hash)
              (make-bdd-var-id)
	      ))
	 (bddvarid-list
	  (if bddvarid-hash
	      (cadr bddvarid-hash)
	      (loop for index from 0 to (1- len)
		    collect
                       (make-bdd-incl-excl-var-id   bddvarid index)
                       ))))
    (unless bddvarid-hash
      (setf (gethash expr *pvs-bdd-hash*)
	    (list bddvarid bddvarid-list))
       (let (( *build-access-var*  nil))  ;; Hassen 06/14/98
      (pushnew (cons expr (make-subrange-inclusive-formula
			   bddvarid-list lo hi))
	       *pvs-bdd-inclusivity-formulas*
	       :test #'(lambda (x y)(tc-eq (car x)(car y)))))
        )
	bddvarid-list))

(defun make-scalar-names (expr)
  #+pvsdebug
  (pvs-message "find in hash table ~a ~a"
    (unparse expr :string t) (gethash expr *pvs-bdd-hash*))
  (let* ((recs (recognizers (find-supertype (type expr))))
	 (len (max 1;;NSH(9.29.95) needed since log 1 2 = 0
		   (ceiling;;noticed by Nick Graham (York)
		    (log (length recs) 2))))
	 (bddvarid-hash (gethash expr *pvs-bdd-hash*))
	 (bddvarid (if bddvarid-hash
		       (car bddvarid-hash)
		       (make-bdd-var-id)))
	 (bddvarid-list
	  (if bddvarid-hash
	      (cadr bddvarid-hash)
	      (loop for index from 0 to (1- len)
		    collect (make-bdd-incl-excl-var-id  bddvarid index))))
	 (format-bitstring (format nil "~~~d,'0b" len)))
    (unless bddvarid-hash
      (setf (gethash expr *pvs-bdd-hash*)
	    (list bddvarid bddvarid-list))
      (when (null (freevars expr))
	(loop for bddnm in (reverse bddvarid-list)
	      as i from 0
	      do (let* ((irecs
			 (loop for rec in recs as j from 0
			       when
			       (eql (digit-char-p
				     (elt (format nil format-bitstring j)
					  i))
				    1)
			       collect
			       (make-application rec expr)))
			(bdd-hashname
			 (make-disjunction irecs)))
		   (setf (gethash bddnm *bdd-pvs-hash*)
			 bdd-hashname))))
      (let (( *build-access-var*  nil));; Hassen 06/14 
	;; create-bool-var in inclusivity
	(pushnew (cons expr (make-scalar-inclusive-formula
			     bddvarid (1- (length recs)) (1- len)))
		 *pvs-bdd-inclusivity-formulas*
		 :test #'(lambda (x y)(tc-eq (car x)(car y))))))
    (if *build-access-var*  
	(make-binding-vars-scalar bddvarid-list) 
        (if *build-arguments* (make-argument-vars-scalar bddvarid-list) 
	    bddvarid-list));; Hassen 06/14/98
    ))


(defun make-binding-vars-scalar (bddvarid-list)
  (mapcar #'(lambda (bddvarid) 
	      (mu-check-bool-var (format nil "b~d" bddvarid)))
    bddvarid-list))


(defun make-argument-vars-scalar (bddvarid-list)
  (mapcar #'(lambda (bddvarid) 
	      (mu-make-bool-var (format nil "b~d" bddvarid)))
    bddvarid-list))



(defun make-scalar-constant-bits (constant) ;; given a scalar constant
    (let* ((recog (recognizer constant))
	 (recs (recognizers
		(find-supertype (type constant))))
	 (len (ceiling
	       (log (length recs) 2)))
	 (bitlist (map 'list
		       #'(lambda (bit)
			   (if (eql (digit-char-p bit) 1)
			       (mu-mk-true)
			       (mu-mk-false)))
		       (format nil
			   (format nil "~~~d,'0b"
			     len)
			 (position recog recs :test #'tc-eq)))))
   (lisp-to-c-list bitlist) 
))
		   

;;NSH(2.1.96); added reverse
;; Klaus noticed this unsoundness since the lsb was being set to the
;;msb of the position bit string.  

;; Hassen tests 06/11/98


(defun make-mu-variable-recognizer-application (expr)
  (let* ((arg (args1 expr))
	 (op (operator expr))
	 (recs (recognizers
		(find-supertype (type arg))))
	 (len (ceiling
	       (log (length recs) 2)))
	 (bddlist  (reverse (make-scalar-names arg)))
         (bddvarid-list bddlist)
	 (format-bitstring (format nil "~~~d,'0b" len)))
    (make-mu-conjunction
     (map 'list
	  #'(lambda (bit bddnm)
             (let ((new-mu-var  (mu-create-bool-var bddnm)))
	      (if (eql (digit-char-p bit) 1)
                   new-mu-var 
	         (mu-mk-not new-mu-var)
                ))
                 )
	  (format nil
	       format-bitstring
	     (position op recs :test #'tc-eq))
	   bddvarid-list))))

;;
;;
;;
;;


(defun make-geq-bdd (bddvarid-list number-rep) ;; bddvarid-list is a lisp list
  (let* ((bddvarid-list-len (length bddvarid-list))
	 (len (length number-rep))
	 (max-length (max bddvarid-list-len len))
	 (padded-reversed-bddvarid-list
	  (pad-false (reverse bddvarid-list)
		     (- max-length bddvarid-list-len)))
	 (padded-reversed-number-rep
	  (pad-false (reverse number-rep)
		     (- max-length len))))
    (make-geq-bdd* padded-reversed-bddvarid-list
		   padded-reversed-number-rep)))

(defun make-geq-bdd* (bdd-list num-rep)
  (if (consp num-rep)
      (if (consp (cdr num-rep))
	  (if (eq (car num-rep) (mu-mk-true))
               (mu-mk-and  (mu-create-bool-var (car bdd-list)) 
                        (make-geq-bdd* (cdr bdd-list)(cdr num-rep)))
               (mu-mk-or (mu-create-bool-var (car bdd-list)) 
                        (make-geq-bdd* (cdr bdd-list)(cdr num-rep))))
	  (if (eq (car num-rep) (mu-mk-true))
	      (car bdd-list)
	      (mu-mk-true)))
      (mu-mk-true)))


(defun make-leq-bdd (bddvarid-list number-rep)
  (let* ((bddvarid-list-len (length bddvarid-list))
	 (len (length number-rep))
	 (max-length (max bddvarid-list-len len))
	 (padded-reversed-bddvarid-list
	  (pad-false (reverse bddvarid-list)
		     (- max-length bddvarid-list-len)))
	 (padded-reversed-number-rep
	  (pad-false (reverse number-rep)
		     (- max-length len))))
    (make-leq-bdd* padded-reversed-bddvarid-list
		   padded-reversed-number-rep)))

(defun make-leq-bdd* (bdd-list num-rep)
  (if (consp num-rep)
      (if (consp (cdr num-rep))
	  (if (eq (car num-rep) (mu-mk-true))
              (mu-mk-or
                    (mu-mk-not (mu-create-bool-var (car bdd-list)))
                       (make-leq-bdd* (cdr bdd-list)(cdr num-rep)))
              (mu-mk-and 
                    (mu-mk-not (mu-create-bool-var (car bdd-list)))
                       (make-leq-bdd* (cdr bdd-list)(cdr num-rep))))
	  (if (eq (car num-rep) (mu-mk-true))
	      (mu-mk-true)
	      (mu-mk-not (car bdd-list) )))
      (mu-mk-true)))


(defun pad-false (list n)
  (if (zerop n)
      list
      (cons (mu-mk-false) (pad-false list (1- n)))))

;;
;;

(defun mu-mk-true()
 (if *build-mu-term* (mu_mk_true_term) (mu_mk_true_formula))
)
                      
(defun mu-mk-false()
 (if *build-mu-term* (mu_mk_false_term) (mu_mk_false_formula))
)


(defun mu-mk-not (fml1) 
   (if *build-mu-term* 
            (mu_mk_not_term  fml1)
            (mu_mk_not_formula  fml1)
    ))

(defun mu-mk-and (fml1 fml2)
 (if *build-mu-term* 
         (mu_mk_and_term  fml1 fml2)
         (mu_mk_and_formula  fml1 fml2)
 ))
       
(defun mu-mk-or (fml1 fml2)
 (if *build-mu-term* 
         (mu_mk_or_term  fml1 fml2)
         (mu_mk_or_formula  fml1 fml2)
 ))
       
(defun mu-mk-equiv (fml1 fml2)
 (if *build-mu-term* 
         (mu_mk_equiv_term  fml1 fml2)
         (mu_mk_equiv_formula  fml1 fml2)
 ))
       
(defun mu-mk-implies (fml1 fml2)
 (if *build-mu-term* 
         (mu_mk_implies_term  fml1 fml2)
         (mu_mk_implies_formula  fml1 fml2)
 ))
       
(defun mu-mk-cof (fml1 fml2)
   (mu_mk_cofactor fml1 fml2)
)


(defun make-mu-application (op-str args-list)
 (mu_mk_curry_application op-str args-list)
)
;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-mu-conjunction (list-fmls)
  (if (null list-fmls) 
      (if *build-mu-term* (mu_mk_true_term) (mu_mk_true_formula))
      (if (cdr list-fmls) 
	  (mu-mk-and  (car list-fmls) (make-mu-conjunction (cdr list-fmls)))
	  (car list-fmls))))


(defun make-mu-disjunction (list-fmls)
  (if (null list-fmls) 
      (if *build-mu-term* (mu_mk_false_term) (mu_mk_false_formula))
    (if (cdr list-fmls) 
	(mu-mk-or (car list-fmls) (make-mu-disjunction (cdr list-fmls)))
      (car list-fmls))))



(defun make-mu-restriction (mu-expr1 mu-expr2)
  (let ((e1 mu-expr1)(e2 mu-expr2))
   (mu-mk-cof e1 e2)))


(defun make-bdd-var-id ()
 (let ((indicevar (funcall *bdd-counter*)))
    indicevar)
)


(defun make-bdd-incl-excl-var-id (bvarid index) 
  (when (and (not (member (cons bvarid index) 
         *incl-excl-var-id-indx-pairs* :test #'equal)) (not (zerop index)))
         (funcall *bdd-counter*) 
         (setq *incl-excl-var-id-indx-pairs* 
                 (cons (cons bvarid index) *incl-excl-var-id-indx-pairs* )))
;;  (let ((new-index (1+ index)))  ;; index may be 0 | yes but bvarid does not correspond to any variable!!!
 (let ((new-index  index))
    (+ bvarid new-index))
)


(defun mu-create-bool-var (bvarid)
  #+pvsdebug
  (pvs-message " ~% term? ~a   acc? ~a    rel-var? ~a   build-arg? ~a ~% "
    *build-mu-term* *build-access-var* *build-rel-var* *build-arguments*)
  (let ((bvarname (if (stringp bvarid) bvarid (format nil "b~d" bvarid))))
    (if (and *build-rel-var* (not *build-access-var*)) 
	(mu-mk-rel-var-dcl  bvarname)
	(if *build-access-var* 
	    (mu-check-bool-var bvarname)
	    (mu-make-bool-var bvarname)))))

(defun mu-mk-rel-var (bvarname)
(mu_mk_rel_var_ (FF:STRING-TO-CHAR* bvarname)))

(defun mu-mk-rel-var-dcl (bvarname)
(mu_mk_rel_var_dcl (FF:STRING-TO-CHAR* bvarname)))

(defun mu-check-bool-var (bvarname)
(mu_check_bool_var (FF:STRING-TO-CHAR* bvarname)))

(defun mu-make-bool-var (bvarname)
 (if *build-mu-term*
  (mu_mk_rel_var_ (FF:STRING-TO-CHAR* bvarname))
  (mu_check_mk_bool_var (FF:STRING-TO-CHAR* bvarname))
))

;;
;;
;; determine if expr is of the form "mu(P,PP)(Q)" or "nu(P,PP)(Q)"
;;  Hassen. 05/02/98
;;

(defun mu-nu-expr-application? (expr)
  (if (application? expr)
      (let ((op (operator expr)))
          (mu-nu-expr? op)
      )
    nil)
)


;; determine if expr is of the form "mu(P,PP)" or "nu(P,PP)"

(defun mu-nu-expr? (expr)
  (if (application? expr)
      (let ((op (operator expr)))
	(and (typep op 'name-expr)
	     (find (string (id op)) '("mu" "nu") :test #'string=)
	     (eq (id (module (declaration op))) '|mucalculus|)
	     (let
		 ((mu-actuals (actuals (module-instance
					(resolution op)))))
	       (and (singleton? mu-actuals)
		    (or (mu-translateable?
			 (type-value (car mu-actuals)))
 (format t "~%Theory mucalculus with type ~a is not model-checkable."
			  (type-value (car mu-actuals))))))))
      nil))


(defun reachable? (expr)
  (and (name-expr? expr)
	      (eq (id expr) '|Reachable|)
	      (eq (id (module (declaration expr)))
		  '|Reachable|)))

(defun reachable-expr? (expr)
  (and (application? expr)
       (let ((op* (operator* expr)))
	 (reachable? op*))))

;;
;;
;;


(defun subrange!? (type)
  (let* ((subrange (type-value (declaration (subrange-res))))
	 (*modsubst* T)
	 (subst (match subrange type nil nil))) ;(break)
    (when (and (not (eq subst 'fail))
	       (every #'(lambda (x)(number-expr? (cdr x))) subst))
      (cons (number (cdr (assoc '|i| subst :test #'same-id)))
	    (number (cdr (assoc '|j| subst :test #'same-id)))))))
	

(defun below!? (type)
  (let* ((below (type-value (declaration (below-res))))
	 (*modsubst* T)
	 (subst (match below type nil nil)))
    (when (and (not (eq subst 'fail))
	       (every #'(lambda (x)(number-expr? (cdr x))) subst))
      (cons 0 (1- (number
		   (cdr (assoc '|i| subst :test #'same-id))))))))

(defun upto!? (type)
  (let* ((upto (type-value (declaration (upto-res))))
	 (*modsubst* T)
	 (subst (match upto type nil nil)))
    (when (and (not (eq subst 'fail))
	       (every #'(lambda (x)(number-expr? (cdr x))) subst))
      (cons 0 (number (cdr (assoc '|i| subst :test #'same-id)))))))

(defun sub-range? (type)
  (or (subrange!? type)
      (below!? type)
      (upto!? type)))
    
;;
;;
;;

(defmethod mu-translateable? ((type type-name))
  (or (tc-eq type *boolean*)
      (and (adt type)
	   (mu-translateable? (adt type)))))

(defmethod mu-translateable? ((type enumtype))
  T)

(defmethod mu-translateable? ((type recordtype))
  (every #'(lambda (x) (mu-translateable? (type x)))
	 (fields type)))

(defmethod mu-translateable? ((type funtype))
  (and (sub-range? (domain type))
       (mu-translateable? (range type))))

(defmethod mu-translateable? ((type subtype))
  (or (sub-range? type)
      (mu-translateable? (supertype type))))

(defmethod mu-translateable? ((type T))
  nil)
 

;;NSH(6.22.98): strict-mu-translateable? rejects non-subrange subtypes
;;and is needed for universal/existential bound variables.  
(defmethod strict-mu-translateable? ((type type-name))
  (or (tc-eq type *boolean*)
      (and (adt type)
	   (strict-mu-translateable? (adt type)))))

(defmethod strict-mu-translateable? ((type enumtype))
  T)

(defmethod strict-mu-translateable? ((type recordtype))
  (every #'(lambda (x) (strict-mu-translateable? (type x)))
	 (fields type)))

(defmethod strict-mu-translateable? ((type funtype))
  (and (sub-range? (domain type))
       (strict-mu-translateable? (range type))))

(defmethod strict-mu-translateable? ((type subtype))
  (sub-range? type))

(defmethod strict-mu-translateable? ((type T))
  NIL)


;;
;;
;;

(defun boolean-array? (type)
  (and (funtype? type)
       (let ((ptype
	      ;(and
	       ;(consp (domain type))
			  (print-type
			   (domain type))
			   ;(car (domain type))
			   ;))
	     ))
	 (and (type-name? ptype)
	      (tc-eq (range type) *boolean*)
	      (memq (id ptype) '(|upto| |below|))
	      (number-expr? (expr (car (actuals (module-instance ptype)))))
	      (if (eq (id ptype) '|upto|)
		  (number (expr (car (actuals (module-instance ptype)))))
		  (1- (number (expr (car (actuals (module-instance ptype)))))))))))
		 


(defun bool-ineq? (expr);;assuming expr is application
  (let ((op (operator expr)))
    (and (name-expr? op)
	 (memq (id op) '(< <= > >=))
	 (eq (mod-id (module-instance (resolution op)))
	     '|bv_arith_nat|)
	 (number-expr? (expr (car (actuals (module-instance (resolution op))))))
	 (id op))))


(defun boolean? (term)
  (tc-eq (type term) *boolean*))
 
(defun all-not-boolean? (term-list)
  (find-if-not #'(lambda (x) (or (tc-eq (type x) *boolean*)
				 (boolean-array? (type x))))
    term-list))
 
(defun all-boolean? (term-list)
  (not (all-not-boolean? term-list)))

;; methods to produce uncurried forms from curried forms
(defmethod get-op ((expr application))
  (let ((op (operator expr)))
    (get-op op)))

(defmethod get-op ((expr expr))
  expr)

(defmethod get-args ((expr application))
  (let* ((args (arguments expr))
	 (op (operator expr))
	 (opargs (get-args op)))
    (append opargs args)))

(defmethod get-args ((expr expr))
  nil)


;;
;; Hassen 05/06/1998
;; reachable-expr? should be of the form Reachable(list-args1)(list-args2)
;; where list-args1 is of length 2 or 3.
;;

(defun lisp-to-c-list (lisp-list)
 (if (consp lisp-list)
         (from-lisp-list-to-c-list  (reverse lisp-list)) 
 ;; reverse to get the right order again
         lisp-list))

(defun from-lisp-list-to-c-list (lisp-list)
  (if (null lisp-list) (empty_list)
       (append_cont (car lisp-list) (from-lisp-list-to-c-list (cdr lisp-list))
  ))
)

;;
;;
;;
;;


(defun mu-translate-from-bdd-list (bddlist)
  (let ((bdds (unless (zerop bddlist)
		(mu-translate-from-bdd-list* (list_first bddlist)))))
    (mapcar #'mu-translate-bdd-cube bdds)))

(defun mu-translate-from-bdd-list* (bddlist &optional result)
  (if (zerop bddlist)
      (nreverse result)
      (mu-translate-from-bdd-list*
       (list_next bddlist)
       (cons (elem_contents bddlist) result))))

(defun mu-translate-bdd-cube (cube)
  (cond ((or (bdd-void? cube)
	     (bdd-x? cube))
	 nil)
	((bdd-term? cube)
	 (list *true*))
	(t (mu-translate-bdd-cube* cube))))

(defun mu-translate-bdd-cube* (cube &optional result)
  (if (bdd-term? cube)
      (nreverse result)
      (let ((bdd-t (bdd_cofactor_pos_ cube))
	    (bdd-e (bdd_cofactor_neg_ cube))
	    (varid (mu_bdd_var_id (bdd_varid cube))))
	(cond ((bdd-0? bdd-e)
	       (mu-translate-bdd-cube* bdd-t (cons varid result)))
	      ((bdd-0? bdd-t)
	       (mu-translate-bdd-cube* bdd-e (cons (list varid) result)))
	      (t (mu-translate-bdd-cube* bdd-t (cons varid result)))))))

;;(defun mu_bdd_var_id  (bdd-id)
;;  (+ *save-bdd-counter* (get_bdd_var_id  bdd-id))
;;)

(defun mu_bdd_var_id  (bdd-id)
(string-to-number  
    (string-left-trim "b" 
        (FF:CHAR*-TO-STRING 
             (get_mu_bool_var_name  bdd-id))))
)


(defun map-bdds-to-ids (bddlist)
 (mapcar #'(lambda (bddpointer) (let ((varid (bdd_varid bddpointer))) 
            (mu_bdd_var_id varid)))
     (get-bdd-ids-from-c-list bddlist)))

(defun get-bdd-ids-from-c-list (clist)
  (let ((first-elem (list_first clist)))
     (real-get-bdd-ids-from-c-list first-elem))
)

(defun real-get-bdd-ids-from-c-list (clist &optional result)
  (if (equal 0 clist)  
        (nreverse result)
        (real-get-bdd-ids-from-c-list
	  (list_next  clist)
           (cons (elem_contents clist)  result)))
)
