;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; 
;; 
;; (compile-file "/project/pvs/pvs2.2/BDD/mu.lisp")(load"/project/pvs/pvs2.2/BDD/mu.fasl")(load"/project/pvs/pvs2.2/BDD/Linux/bdd/bdd.so")(load"/project/pvs/pvs2.2/BDD/Linux/mu/mu.so")

(in-package 'pvs)  

(require :foreign)

;;;;;;;;;;;;;;;;;
;;;  Formula  ;;;
;;;;;;;;;;;;;;;;;

(ff:defforeign 'mu_mk_false_formula)
;;; Formula mu_mk_false_formula (void)
(ff:defforeign 'mu_mk_true_formula)
;;; Formula mu_mk_true_formula (void)
(ff:defforeign 'mu_mk_bool_var)
;;; Formula mu_mk_bool_var (char *name)
(ff:defforeign 'mu_check_bool_var)
;;; Formula mu_check_bool_var
(ff:defforeign 'mu_check_mk_bool_var)
;;; Formula mu_check_mk_bool_var
(ff:defforeign 'mu_mk_ite_formula)
;;; Formula mu_mk_ite_formula (Formula cond, Formula then_part, Formula else_part)
(ff:defforeign 'mu_mk_curry_application)
;;; Formula mu_mk_curry_application (Term R, LIST subs, int curried)

(ff:defforeign 'mu_mk_forall) ;; (listvars fml1) always formula
(ff:defforeign 'mu_mk_exists) ;; (listvars fml1) always formula

(ff:defforeign 'mu_mk_implies_formula) ;; (fml1 fml2)
(ff:defforeign 'mu_mk_equiv_formula);; (fml1 fml2)
(ff:defforeign 'mu_mk_xor_formula);; (fml1 fml2)
(ff:defforeign 'mu_mk_or_formula);; (fml1 fml2)
(ff:defforeign 'mu_mk_and_formula);; (fml1 fml2)
(ff:defforeign 'mu_mk_not_formula);; (fml1 fml2)
(ff:defforeign 'mu_mk_cofactor);; (fml1 fml2) always formula

;;;;;;;;;;;;;;;
;;;  Term   ;;;
;;;;;;;;;;;;;;;
(ff:defforeign 'mu_mk_abstraction)
;;; Term mu_mk_abstraction (LIST vars, Formula f1)
(ff:defforeign 'mu_mk_l_fixed_point)
;;; Term mu_mk_fixed_point 
(ff:defforeign 'mu_mk_g_fixed_point)
;;; Term mu_mk_g_fixed_point 
(ff:defforeign 'mu_mk_reach)
;;; Term mu_mk_reach (Term Next, Term S0, Term Inv)
(ff:defforeign 'mu_mk_rel_var_dcl)
;;; Term mu_mk_rel_var_dcl (char *name) 
(ff:defforeign 'mu_mk_rel_var)
;;; Term  mu_mk_rel_var (R_Interpret Ip, char *name)
(ff:defforeign 'mu_mk_true_term)
;;; Term  mu_mk_true_term (void)
(ff:defforeign 'mu_mk_false_term)
;;; Term  mu_mk_false_term (void)

(ff:defforeign 'mu_mk_not_term) ;; (fml1)
(ff:defforeign 'mu_mk_and_term) ;; (fml1 fml2)
(ff:defforeign 'mu_mk_or_term) ;; (fml1 fml2)
(ff:defforeign 'mu_mk_equiv_term) ;; (fml1 fml2)
(ff:defforeign 'mu_mk_implies_term) ;; (fml1 fml2)
(ff:defforeign 'mu_mk_xor_term) ;; (fml1 fml2)



;;;;;;;;;;;;;;;;;;;
;;;  Lists      ;;;
;;;;;;;;;;;;;;;;;;;
;; 
(ff:defforeign 'append_cont)

;;;
;;; Flags

(ff:defforeign 'set_mu_bdd_ordering)
(ff:defforeign 'set_mu_warnings)
(ff:defforeign 'set_mu_simplify_frontier)
(ff:defforeign 'set_mu_verbose)
(ff:defforeign 'set_mu_bdd_use_neg_edges)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main function   ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(ff:defforeign 'mu_init)
(ff:defforeign 'mu_quit)
(ff:defforeign 'modelcheck_formula)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global variables     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *build-mu-term* nil) ;; t when converting a term and nil when converting a formula
(defvar *build-rel-var* nil) ;; t when mu_mk_rel_var and nil when mu_mk_bool_var
(defvar *build-access-var* nil) ;; t when mu_check_bool_var and nil othrwise.


(addrule 'musimp () ((fnums *)
		     (dynamic-ordering? nil))
	 (musimp-fun fnums dynamic-ordering?)
	 "MU Calculus Simplification .
  Dynamic ordering means the BDD package can reorder literals
  to reduce BDD size."
	 "~%Applying musimp,")


(defun musimp-fun (&optional fnums dynamic-ordering?)
  #'(lambda (ps)(run-musimp ps fnums dynamic-ordering?)))


(defun run-musimp (ps fnums dynamic-ordering?)
  (let* ((*pvs-bdd-hash* (make-hash-table
			  :hash-function 'pvs-sxhash :test 'tc-eq))
	 (*bdd-pvs-hash* (make-hash-table))
	 (*pvs-bdd-inclusivity-formulas* nil)
	 (*bdd-counter* *bdd-counter*)
	 (*recognizer-forms-alist* nil)
         (*mu-subtype-list* nil)
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
	 (list-of-conjuncts (translate-from-bdd-list  
			     (bdd_sum_of_cubes mu-output 1))) 
	 (lit-list (mapcar #'(lambda (conj)
			       (mapcar #'(lambda (lit)
					   (if (consp lit)
					     (gethash (car lit) *bdd-pvs-hash*)
					     (make-negation
						(gethash lit *bdd-pvs-hash*))))
				 conj))
		     list-of-conjuncts))
	 )
    (add-bdd-subgoals ps sforms lit-list remaining-sforms)
    )
  )


;;
;;
;; run-pvsmu
;;


(defun run-pvsmu (mu-formula dynamic-ordering?)
;;   (set_mu_bdd_ordering (if dynamic-ordering? 1 0))
   (set_mu_warnings 0)
   (set_mu_simplify_frontier 1)
   (set_mu_verbose 1)
   (mu-interpret-formula mu-formula)
 )


;;
;; mu-interpret-formula
;;
;;

(defun mu-interpret-formula (mu-formula)
 (let ((result-formula (modelcheck_formula mu-formula)))
  result-formula
))

;;
;;
;; convert-pvs-to-mu-formula
;; calls convert-pvs-to-mu-formula*
;;

(defun convert-pvs-to-mu (expr) ;;expr must be of boolean type
  (let* ((*bound-variables* nil)
	 (*mu-nu-lambda-bindings-list* nil)
	 (mu-expr (convert-pvs-to-mu-formula expr))) ;; Formulas come first
 mu-expr)
)


;;
;;  a mu-formula has the following structure:
;;  Formula ::=   Var
;;              | not Formula
;;              | forall(List_vars) .Formula
;;              | exists(List_vars) .Formula
;;              | Formula {or,and,=>,<=>,cof} Formula           
;;              | Term (List_Formula)
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
 (let ((*build-mu-term* nil) ;; building a Formula
       (mu-expr (convert-pvs-to-mu* expr)))
 mu-expr)
)


(defun convert-pvs-to-mu-term (expr) 
 (let ((*build-mu-term* t) ;; building a Term
       (mu-expr (convert-pvs-to-mu* expr)))
 mu-expr)
)


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
      (let ((constraints (collect-type-constraints-step expr))
	    (*mu-subtype-list* (cons expr *mu-subtype-list*)))
	(loop for x in constraints do
	      (push (cons expr (convert-pvs-to-mu* x))
		    *pvs-bdd-inclusivity-formulas*))
	(format t "~%Added constraints: ~a" constraints)))
    (call-next-method)))


(defmethod convert-pvs-to-mu* ((expr list))
 (if (null expr) (NULL_LIST)
      (append_cont (convert-pvs-to-mu* (car expr))
          (convert-pvs-to-mu* (cdr expr)))
  )
)


(defmethod convert-pvs-to-mu* ((expr cases-expr))
  (convert-pvs-to-mu* (translate-cases-to-if expr))
)


(defmethod convert-pvs-to-mu* ((expr expr))  ;;NSH(8.17.95) added find-supertype
  (cond ((tc-eq expr *true*) (mu-mk-true))
	((tc-eq expr *false*) (mu-mk-false))
	((reachable? expr) (reach_pf))
	((and (binding-expr? expr)
	      (every #'(lambda (x)(mu-translateable? (find-supertype (type x))))
		     (bindings expr)) ;;NSH(4.24.95)
	      )
             (convert-pvs-to-mu-binding-expr expr))
        ((scalar-constant? expr)     (make-scalar-constant-bits expr)) ;;NSH(6.11.95)
        ((scalar?  expr)  (uncurry-application-and-mu-convert expr))
	((recordtype? (find-supertype (type expr))) ;;(break)
	 (convert-pvs-to-mu*
	  (loop for decl in (fields (find-supertype (type expr)))
		       collect (beta-reduce
				(make-field-application
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
	(t (make-mu-variable expr)))
)


(defmethod convert-pvs-to-mu* ((expr application))
  (let* ((old-expr expr)
	 (*lift-if-updates* T)
	 (expr (if (and (boolean? expr)
			(equality? expr))
		   (lift-if-expr expr)
		   expr)))
    (cond ((disjunction? expr) (convert-pvs-to-mu-disjunction expr))
          ((conjunction? expr) (convert-pvs-to-mu-conjunction expr))  
          ((iff? expr)  (convert-pvs-to-mu-iff expr))
          ((implication? expr)  (convert-pvs-to-mu-implication expr))
          ((not-expr? expr)   (convert-pvs-to-mu-negation expr))
          ((branch? expr)  (convert-pvs-to-mu-branch  expr))
          ((inequality? expr)  (convert-pvs-to-mu-inequality  expr))
          ((equality? expr) (convert-pvs-to-mu-equality expr))
          ((mu-nu-expr-application? expr) (convert-pvs-to-mu-nu-application  expr))
          ((mu-nu-expr? expr) (convert-pvs-to-mu-nu-expression expr))
          ((reachable-expr? expr) (convert-pvs-to-mu-reachable expr))
          ((NOT (tc-eq (find-supertype (type expr)) *boolean*))
            (call-next-method))
          (t;; Other application like R(x,y)
            (uncurry-application-and-mu-convert expr))
)))



(defmethod convert-pvs-to-mu* ((expr number-expr))
  (convert-number (number expr)))



(defun convert-pvs-to-mu-binding-expr (expr)
         (let* ((expr-bindings (bindings expr))
		(expr-bindings
		 (mapcar #'make-variable-expr expr-bindings))
		(old-inclusivities *pvs-bdd-inclusivity-formulas*)
		(boundvars (let ((*build-access-var* t)) 
                        (convert-pvs-to-mu* expr-bindings)))
		(*bound-variables* (append expr-bindings
					   *bound-variables*))
		(new-inclusivities
		 (ldiff *pvs-bdd-inclusivity-formulas*
			old-inclusivities))
		(boundexpr
		  (convert-pvs-to-mu-formula (expression expr))
		))
	   (setq *pvs-bdd-inclusivity-formulas*  ;;NSH(8.17.95): to collect
		                            ;;subtypes from inside binding expr.
		 (set-difference *pvs-bdd-inclusivity-formulas*
				 new-inclusivities
				 :key #'car
				 :test #'tc-eq))
      (cond ((forall-expr? expr) 
                (convert-pvs-to-mu-forall boundvars boundexpr new-inclusivities))
            ((exists-expr? expr) 
                (convert-pvs-to-mu-exists boundvars boundexpr new-inclusivities))
            ((lambda-expr? expr) (convert-pvs-to-mu-lambda boundvar boundexpr))
            ))
)




(defun convert-pvs-to-mu-forall (expr boundvars boundexpr new-inclusivities)
  (if new-inclusivities
         (mu_mk_forall  boundvars
                 (mu-mk-implies
                    (make-mu-conjunction  (mapcar #'cdr new-inclusivities)) boundexpr))
         (mu_mk_forall  boundvars boundexpr)
  ))


(defun convert-pvs-to-mu-exists (expr boundvars boundexpr new-inclusivities)
  (if new-inclusivities
         (mu_mk_exists (mu_exist) boundvars
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
	(cond ((eq result (mu-mk-true))
	       (convert-pvs-to-mu* (then-part expr)))
	      ((eq result (mu-mk-false))
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
      (let* ((exprargs (arguments expr))
	     (argsfml (convert-pvs-to-mu* exprargs)) 
	     (muop (operator expr))
	     (mu-or-nu (string (id (operator muop))))
	     (muargs1bindgs (bindings (args1 muop)))
	     (*mu-nu-lambda-bindings-list*
	      (append muargs1bindgs
		      *mu-nu-lambda-bindings-list*))
	     (muarg1fml (let ((*build-rel-var* t)) 
                                  (convert-pvs-to-mu*
			 (mapcar #'make-variable-expr muargs1bindgs))))
	     (muarg2expr (expression (args1 muop)))
	     (muarg2fml (convert-pvs-to-mu-term muarg2expr))
	     (muexprfml
               (mu_mk_curry_application
                      (if (string= mu-or-nu "mu") 
                          (mu_mk_l_fixed_point muarg1fml muarg2fml)
                          (mu_mk_g_fixed_point muarg1fml muarg2fml))  argsfml))
          )
	muexprfml )
)


(defun convert-pvs-to-mu-nu-expression (expr)
      (let* ((muop (operator expr))
	     (mu-or-nu (string (id muop)))
	     (muargs1bindgs (bindings (args1 expr)));;args1muop islambda-expr
	     (muarg1fml 
                    (let ((*build-rel-var* t)) (convert-pvs-to-mu*
			 (mapcar #'make-variable-expr muargs1bindgs))))
	     (*mu-nu-lambda-bindings-list*;;NSH(6.23.95)
	      (append muargs1bindgs
		      *mu-nu-lambda-bindings-list*))
	     (muarg2expr (expression (args1 expr)))
	     (muarg2str (convert-pvs-to-mu-term muarg2expr))
	     (muexprstr
                 (if (string= mu-or-nu "mu") 
                          (mu_mk_l_fixed_point muarg1fml muarg2fml)
                          (mu_mk_g_fixed_point muarg1fml muarg2fml))) 
                    )
	muexprstr)
)



(defun convert-pvs-to-mu-reachable (expr)
  (let* ((mu-list-args (convert-pvs-to-mu-term (arguments* expr)))
         (reach-list-arguments (car mu-list-args))
                        ;; should be a list of terms
         (other-arguments (cdr mu-list-args))
         (nbofargs (length (arguments reach-list-arguments )))
         (mu-reach-expr
   (cond ((equal 2 nbofargs) (mu_mk_reach (nth 0 mu-list-args) 
                             (nth 1 mu-list-args) (mu-mk-true) ))
         ((equal 3 nbofargs) (mu_mk_reach (nth 0 mu-list-args) 
                 (nth 1 mu-list-args) (nth 2 mu-list-args) ))
              )))
   (mu_mk_curry_application mu-reach-expr other-arguments))
)




(defun convert-pvs-to-mu-equality (expr)
 (cond
   ((and (funtype? (find-supertype (type (args1 expr))))
        (mu-translateable? (type (args1 expr))))
    (convert-pvs-to-mu-equality-with-funtype-in-arg expr)) 
   ((or (branch? (args1 expr));;NSH(2.2.96): added cases?
       (cases-expr? (args1 expr)));;to this and next case, else      
    (convert-pvs-to-mu-equality-with-case-args expr )) 
   ((or (branch? (args2 expr))
       (cases-expr? (args2 expr)))
    (convert-pvs-to-mu-equality-with-case-arg (make-equality (args2 expr) (args1 expr)))) 
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
 )
)                 


(defmethod convert-number (number)
  (let* ((len (max 1 (ceiling (log (1+ number) 2))))
         (format-bitstring (format nil "~~~d,'0b" len)) 
         (bit-string (format nil format-bitstring number))
         (lisp-list-values (nreverse  ;lsb first: little-endian
               (loop for i from 0 to (1- len)
	             collect (if (eql (digit-char-p (elt bit-string i)) 1)
		                   (mu-mk-true) (mu-mk-false))))))
 (from-lisp-list-to-c-list lisp-list-values))
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


(defun make-mu-variable (expr)
 (pvs-message "make-expr" )
  (let ((bddvarid (gethash expr *pvs-bdd-hash*)))
       (cond ((null bddvarid)
              (cond ((sub-range? (type expr)) (make-subrange-names expr))
                    ((scalar? expr) (make-scalar-names expr))
                    (t (let ((new-bddvarid
		       (if (recognizer-application? expr)
			   (make-mu-variable-recognizer-application expr)
			   (make-bdd-var-id))))
		  (setf (gethash expr *pvs-bdd-hash*)
			new-bddvarid)
		  (when (null (freevars expr))
		    (setf (gethash new-bddvarid *bdd-pvs-hash*)
			  expr))
		  (mu-create-bool-var new-bddvarid)))))
                 (t   (let ((bddvarid  (if (consp bddvarid)(cadr bddvarid) bddvarid)))
                                 (mu-create-bool-var bddvarid)))
                 ))
)

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
			    (args-list (convert-pvs-to-mu* args*)))       
                        (mk_mu_application op-str args-list))                                    
		      (if (and (mu-translateable? (type (operator expr)))
			       (not (number-expr? (argument expr))))
			  (let* ((args-list (convert-pvs-to-mu* (argument expr)))
				 (args-list (if (listp args-list) args-list
					      (list args-list))))
			   ;; (decode-array (operator expr) (nreverse args-list))
                              (make-mu-variable expr)) ;; Hassen 05/07/98
			  (make-mu-variable expr))))
		(make-mu-variable expr)))
	(convert-pvs-to-mu* expr))))


(defun decode-array (op args)
  (let* ((format-string (decode-array-format args))
	 (oplist
	  (loop for i from 0 to (1- (expt 2 (length args)))
		collect
		(convert-pvs-to-mu*
		 (make-application op
		   (make-number-expr i)))))
	 (len (if (consp oplist)
		  (if (consp (car oplist))
		      (length (car oplist))
		      1)
		  0)))
    (loop for j from 0 to (1- len)
	  collect
	  (apply #'format (cons nil
				(cons format-string
				      (mapcar #'(lambda (x)(nth j x))
					oplist)))))
    ))
					  
(defun decode-array-format (args)
  (if (consp args)
      (format nil "(~a ? ~a : ~a)" (car args)
	      (decode-array-format (cdr args))
	      (decode-array-format (cdr args)))
      "~a")
)


;;
;;
;; Hassen


(defun decode-array (op args)
  (let* ((bindexpr (decode-array-format args))
	 (oplist
	  (loop for i from 0 to (1- (expt 2 (length args)))
		collect
		(convert-pvs-to-mu*
		 (make-application op
		   (make-number-expr i)))))
	 (len (if (consp oplist)
		  (if (consp (car oplist))
		      (length (car oplist))
		      1)
		  0))) ;; could not be 0! Hassen
     (if (equal 0 len) bindexpr
           (mu_mk_curry_application  
                  (mu_mk_abstraction () bindexpr)  
                        (from-lisp-list-to-c-list oplist)))
))


					  
(defun decode-array-format (args)
  (if (consp args)
      (mk_ite_formula (car args) (decode-array-format (cdr args)) 
                   (decode-array-format (cdr args)))
      (mu-create-bool-var  (funcall *bdd-counter*)))))
)

;;
;;
;;
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
	     (scalar1
	      (if (bind-decl? exprarg1)
		  (make-variable-expr exprarg1)
		  exprarg1))
	     (scalar2
	      (if (bind-decl? exprarg2)
		  (make-variable-expr exprarg2)
		  exprarg2))
	     (muvarlist1 (make-scalar-names scalar1))
	     (muvarlist2 (make-scalar-names scalar2))
	     )
	(make-mu-conjunction
	 (mapcar #'(lambda (x y) (mu-mk-equiv x y)
                  ) 
	   muvarlist1 muvarlist2)
              )))



(defun convert-pvs-to-mu-equality-with-recordtype-in-arg (expr) 
      (let* ((type (find-supertype (type (args1 expr))))
	     (fields (fields type))
	     (args1-list
	      (loop for decl in fields
		    collect (beta-reduce
			     (make-field-application
			      (id decl) (args1 expr)))))
	     (args2-list
	      (loop for decl in fields
		    collect (beta-reduce
			     (make-field-application
			      (id decl) (args2 expr)))))
	     (equality-bdds
	      (loop for x in args1-list
		    as y in args2-list
		    collect
		    (convert-pvs-to-mu*
		     (make-equality x y)))))
	(make-mu-conjunction equality-bdds)))


(defun convert-pvs-to-mu-equality-with-funtype-mu-trans-supertype-in-arg (expr) 
      (let* ((type (find-supertype (type (args1 expr))))
	     (range (sub-range? (domain type)))
	     (lo (car range))
	     (hi (cdr range))
	     (args1-list
	      (convert-pvs-to-mu* (args1 expr)))
	     (args2-list
	      (convert-pvs-to-mu* (args2 expr)))
	     (equality-bdds
	      (loop for x in args1-list
		    as y in args2-list
		    collect
                    (mu-mk-equiv x y)   
                   )))
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


 
(defun convert-pvs-to-mu-equality-with-case-args (expr) 
  (let ((arg1 (if (cases-expr? (args1 expr))
		      (translate-cases-to-if (args1 expr))
		      (args1 expr))))
	(convert-pvs-to-mu*
	 (make-if-expr (condition arg1)
		       (make-equality (then-part arg1)
				      (args2 expr))
		       (make-equality (else-part arg1)
				      (args2 expr))))))


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
					(make-field-application x expr))
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
          (mu-mk-not (mu-create-bool-var (make-bdd-incl-excl-var-id bvar 0)))
           )
      (let ((bvarbit (make-bdd-incl-excl-var-id bvar index))
	    (bit (logbitp index num))
	    (rest-fmla
	     (make-scalar-inclusive-formula bvar num (1- index))))
	(if bit
            (mu-mk-or 
               (mu-mk-not  bvarbit) rest-fmla)
            (mu-mk-and 
               (mu-mk-not  bvarbit) rest-fmla)
             ))))

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
      (pushnew (cons expr (make-subrange-inclusive-formula
			   bddvarid-list lo hi))
	       *pvs-bdd-inclusivity-formulas*
	       :test #'(lambda (x y)(tc-eq (car x)(car y)))))
	bddvarid-list))
	

(defun make-scalar-names (expr)
  (let* ((recs (recognizers
		(find-supertype (type expr))))
	 (len (max 1   ;;NSH(9.29.95) needed since log 1 2 = 0
		   (ceiling  ;;noticed by Nick Graham (York)
		    (log (length recs) 2))))
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
		     (make-bdd-incl-excl-var-id  bddvarid index)
                        )))
	 (format-bitstring (format nil "~~~d,'0b" len)))
    (unless bddvarid-hash
      (setf (gethash expr *pvs-bdd-hash*)
	    (list bddvarid bddvarid-list))
      (when (null (freevars expr))
	(loop for bddnm in (reverse bddvarid-list)
	      as i from 0
	      do
	      (let* ((irecs
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
      (pushnew (cons expr (make-scalar-inclusive-formula
			   bddvarid (1- (length recs)) (1- len)))
	       *pvs-bdd-inclusivity-formulas*
	       :test #'(lambda (x y)(tc-eq (car x)(car y)))))
    bddvarid-list))

(defun make-scalar-constant-bits (constant) ;;given a scalar constant
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
   (from-lisp-list-to-c-list bitlist) 
))
		   

(defun make-mu-variable-recognizer-application (expr)
  (let* ((arg (args1 expr))
	 (op (operator expr))
	 (recs (recognizers
		(find-supertype (type arg))))
	 (len (ceiling
	       (log (length recs) 2)))
	 (bddvarid-list (reverse (convert-pvs-to-mu* arg)))
	 (format-bitstring (format nil "~~~d,'0b" len)))
    ;;NSH(6.1.96): moved reverse here from below.
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

;;NSH(2.1.96); added reverse
;; Klaus noticed this unsoundness since the lsb was being set to the
;;msb of the position bit string.  


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
      (cons mu-mk-false (pad-false list (1- n)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-mu-conjunction (list-fmls &optional resfml)
 (if (null list-fmls) (if *build-mu-term* (mu_mk_true_term) (mu_mk_true_formula)) 
       (mu-mk-and  (car list-fmls) (make-mu-conjunction (cdr list-fmls)))
 )
)  


(defun make-mu-disjunction (list-fmls &optional resfml)
 (if (null list-fmls) (if *build-mu-term* (mu_mk_false_term) (mu_mk_false_formula))
       (mu-mk-or (car list-fmls) (make-mu-disjunction (cdr list-fmls)))
 )
)  


(defun make-mu-restriction (mue-xpr1 mu-expr2)
 (mu-mk-cof mue-xpr1 mu-expr2)
)


(defun make-bdd-var-id ()
 (let ((indicevar (funcall *bdd-counter*)))
    indicevar)
)


(defun make-bdd-incl-excl-var-id (bvarid index) 
  (let ((new-index (1+ index))  ;; index may be 0
        (update-bdd-counter (funcall *bdd-counter*)))
    (+ bvarid new-index))
)


(defun mu-create-bool-var (bvarid)
 (pvs-message (format nil "~d " bvarid))
 (let ((bvarname (format nil "b~a" bvarid)))
    (if *build-rel-var*  
            (mu_mk_rel_var_dcl bvarname)
          (if *build-access-var* 
              (mu_check_bool_var bvarname)
              (mu-make-bool-var bvarname))
       ))
)


(defun mu-make-bool-var (bvarname)
 (pvs-message "goes in")
;;  (mu_check_bool_var bvarname)
  (pvs-message "never come back")
 (mu_check_mk_bool_var bvarname)
)

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

(defun from-lisp-list-to-c-list (lisp-list)
  (if (null lisp-list) (NULL_LIST)
       (append_cont (car lisp-list) (from-lisp-list-to-c-list (cdr lisp-list))
  ))
)
