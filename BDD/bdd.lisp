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

(defmacro null-list? (list) `(= (null_list_p ,list) 1))
(defmacro bdd-void? (bdd) `(= (bdd_void_p ,bdd) 1))
(defmacro bdd-1? (bdd) `(= (bdd_1_p ,bdd) 1))
(defmacro bdd-0? (bdd) `(= (bdd_0_p ,bdd) 1))
(defmacro bdd-x? (bdd) `(= (bdd_x_p ,bdd) 1))
(defmacro bdd-term? (bdd) `(= (bdd_term_p ,bdd) 1))

(defmacro bdd-lit? (bdd) `(= (bdd_lit_p ,bdd) 1))
(defmacro bdd-poslit? (bdd) `(= (bdd_poslit_p ,bdd) 1))
(defmacro bdd-neglit? (bdd) `(= (bdd_neglit_p ,bdd) 1))

(defmacro bdd-equal? (bdd1 bdd2)
  `(/= (bdd_equal_p ,bdd1 ,bdd2) 0))

(defmacro bdd-or (bdd1 bdd2)
  (let ((mbdd1 (gensym)) (mbdd2 (gensym)))
    `(let ((,mbdd1 ,bdd1) (,mbdd2 ,bdd2))
       (prog1 (bdd_or ,mbdd1 ,mbdd2)
	 (bdd_free ,mbdd1) (bdd_free ,mbdd2)))))

(defmacro bdd-implies (bdd1 bdd2)
  (let ((mbdd1 (gensym)) (mbdd2 (gensym)))
    `(let ((,mbdd1 ,bdd1) (,mbdd2 ,bdd2))
       (prog1 (bdd_implies ,mbdd1 ,mbdd2)
	 (bdd_free ,mbdd1) (bdd_free ,mbdd2)))))

(defmacro bdd-and (bdd1 bdd2)
  (let ((mbdd1 (gensym)) (mbdd2 (gensym)))
    `(let ((,mbdd1 ,bdd1) (,mbdd2 ,bdd2))
       (prog1 (bdd_and ,mbdd1 ,mbdd2)
	 (bdd_free ,mbdd1) (bdd_free ,mbdd2)))))

(defmacro bdd-equiv (bdd1 bdd2)
  (let ((mbdd1 (gensym)) (mbdd2 (gensym)))
    `(let ((,mbdd1 ,bdd1) (,mbdd2 ,bdd2))
       (prog1 (bdd_equiv ,mbdd1 ,mbdd2)
	 (bdd_free ,mbdd1) (bdd_free ,mbdd2)))))

(defmacro bdd-not (bdd)
  (let ((mbdd (gensym)))
    `(let ((,mbdd ,bdd))
       (prog1 (bdd_not ,mbdd)
	 (bdd_free ,mbdd)))))

(defmacro bdd-ite (bdd1 bdd2 bdd3)
  (let ((mbdd1 (gensym)) (mbdd2 (gensym)) (mbdd3 (gensym)))
    `(let ((,mbdd1 ,bdd1) (,mbdd2 ,bdd2) (,mbdd3 ,bdd3))
       (prog1 (bdd_ite ,mbdd1 ,mbdd2 ,mbdd3)
	 (bdd_free ,mbdd1) (bdd_free ,mbdd2) (bdd_free ,mbdd3)))))


(addrule 'bddsimp () ((fnums *)
		      (dynamic-ordering? nil)
		      (irredundant? t))
  (bddsimp-fun fnums dynamic-ordering? irredundant?)
  "Propositional simplification using Binary Decision Diagrams (BDDs).
  Dynamic ordering means the BDD package can reorder literals
  to reduce BDD size.
  See also PROP."
  "~%Applying bddsimp,")

(defun bddsimp-fun (&optional fnums dynamic-ordering? irredundant?)
  #'(lambda (ps)
      (run-bddsimp ps fnums dynamic-ordering? irredundant?)))

(defun run-bddsimp (ps fnums dynamic-ordering? irredundant?)
  (let* ((sforms (s-forms (current-goal ps)))
	 (selected-sforms (select-seq sforms fnums)))
    (cond ((null selected-sforms)
	   (values 'X nil nil))
	  (t (unless *bdd-initialized* (bdd_init))
	     (if dynamic-ordering?
		 (set_bdd_do_dynamic_ordering 1)
		 (set_bdd_do_dynamic_ordering 0))
	     (let* ((remaining-sforms (delete-seq sforms fnums))
		    (conjuncts (bddsimp-conjuncts selected-sforms
						  irredundant?)))
	       (cond ((bdd-interrupted?)
		      (format t "~%BDD Simplifier interrupted")
		      (values 'X nil))
		     (t
		      (multiple-value-prog1
		       (add-bdd-subgoals ps sforms conjuncts remaining-sforms)
		       (unless *bdd-initialized* (bdd_quit))))))))))

(defun bddsimp-conjuncts (selected-sforms irredundant?)
  (let* ((*pvs-bdd-hash* (make-pvs-hash-table))
	 (*bdd-pvs-hash* (make-hash-table))
	 (*bdd-counter* (let ((x 0)) #'(lambda () (incf x))))
	 (*recognizer-forms-alist* nil)
	 (sforms-bdd (make-sforms-bdd selected-sforms))
	 (list-of-conjuncts (translate-from-bdd-list
			     (bdd_sum_of_cubes sforms-bdd
					       (if irredundant? 1 0)))))
    (from-bdd-list-to-pvs-list list-of-conjuncts)))


(defvar *ignore-boolean-equalities?* nil)

(defun bdd-simplify (expr &optional ignore-boolean-equalities?)
  (unless *bdd-initialized* (bdd_init))
  (let* ((*pvs-bdd-hash* (make-pvs-hash-table))
	 (*bdd-pvs-hash* (make-hash-table))
	 (*recognizer-forms-alist* nil)
	 (*ignore-boolean-equalities?* ignore-boolean-equalities?)
	 (bdd (bdd_not (translate-to-bdd expr)))
	 (ebdd (add-enum-bdds bdd *recognizer-forms-alist*))
	 (bdd-list (translate-from-bdd-list (bdd_sum_of_cubes ebdd 1)))
	 (pvs-list (from-bdd-list-to-pvs-list (nreverse bdd-list))))
    (cond ((bdd-interrupted?)
	   (format t "~%BDD Simplifier interrupted")
	   expr)
	  (t
	   (prog1 (make!-conjunction*
		   (mapcar #'make!-disjunction* pvs-list))
	     (unless *bdd-initialized* (bdd_quit)))))))

(defun from-bdd-list-to-pvs-list (list-of-conjuncts)
  (init-hash-tables) ;; definition in mu.lisp
  (let* ((nconjuncts (simplify-rec-conjuncts list-of-conjuncts))
	 (lit-list (mapcar #'(lambda (conj)
			       (mapcar
				   #'(lambda (lit)
				       (if (consp lit)
					   (gethash (car lit) *bdd-pvs-hash*)
					   (make-negation
					    (gethash lit *bdd-pvs-hash*))))
				 conj))
		     nconjuncts)))
    (assert (hash-table-p *pvs-bdd-hash*))
    lit-list))

;;; With enum types (and simple datatypes), the list-of-conjuncts
;;; returns negative information.  For example, if the enumeration type is
;;; {a, b, c}, the conjuncts will have the form
;;; NOT a?(x)
;;; NOT b?(x)  a?(x)
;;; a?(x)  b?(x)
;;; In the second line, the a?(x) is not needed, and the third line should be
;;; NOT c?(x)
(defun simplify-rec-conjuncts (list-of-conjuncts)
  (mapcar #'simplify-rec-conjuncts* list-of-conjuncts))

(defun simplify-rec-conjuncts* (conjuncts)
  ;; We use *recognizer-forms-alist* to simplify the conjuncts
  (dolist (rec-form *recognizer-forms-alist*)
    (let* ((rec-lits (mapcar #'cdr (cdr rec-form)))
	   (pos-lit (find-if #'(lambda (x) (memq x conjuncts)) rec-lits)))
      (if pos-lit
	  (setq conjuncts
		(remove-if #'(lambda (x)
			       (and (consp x) (memq (car x) rec-lits)))
		  conjuncts))
	  ;; No pos-lits, see if we cover all but one neg-lit
	  (let* ((neg-lits (remove-if (complement
				       #'(lambda (x)
					   (and (consp x)
						(memq (car x) rec-lits))))
			     conjuncts))
		 (stype (find-supertype (type (car rec-form))))
		 (constr-len (if (cotupletype? stype)
				 (length (types stype))
				 (length (constructors stype)))))
	    (when (= (length neg-lits) (1- constr-len))
	      (let ((pos-lit (find-if #'(lambda (x)
					  (not (assq x neg-lits)))
			       rec-lits)))
		(when pos-lit
		  (setq conjuncts
			(replace-neg-lits-with-pos-lit
			 conjuncts neg-lits pos-lit)))))))))
  conjuncts)

(defun replace-neg-lits-with-pos-lit (conjuncts neg-lits pos-lit &optional new)
  (if (null conjuncts)
      (nreverse new)
      (if (and (consp (car conjuncts))
	       (memq (car conjuncts) neg-lits))
	  (replace-neg-lits-with-pos-lit
	   (cdr conjuncts) neg-lits nil (if pos-lit (cons pos-lit new) new))
	  (replace-neg-lits-with-pos-lit
	   (cdr conjuncts) neg-lits pos-lit (cons (car conjuncts) new)))))

(defun add-bdd-subgoals (ps sforms conjuncts remaining-sforms)
  (let ((subgoals
	 (mapcar #'(lambda (c)
		     (create-bdd-subgoal c ps sforms remaining-sforms))
	   conjuncts)))
    (if (and (singleton? subgoals)
	     (subsetp (s-forms (car subgoals)) sforms)
	     (subsetp sforms (s-forms (car subgoals))))
	(values 'X nil nil)
	(values '? subgoals))))

(defun create-bdd-subgoal (conjunct ps sforms remaining-sforms)
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

(defun make-sforms-bdd (selected-sforms &optional bdd)
  (if (null selected-sforms)
      (add-enum-bdds bdd *recognizer-forms-alist*)
      (let* ((fbdd (translate-to-bdd (formula (car selected-sforms))))
	     (negbdd (bdd_not fbdd)))
	(make-sforms-bdd (cdr selected-sforms)
			 (if bdd
			     (bdd_and bdd negbdd)
			     negbdd)))))

(defun add-enum-bdds (bdd rec-form-alist &optional enum-bdds)
  (if (null rec-form-alist)
      (if enum-bdds
	  (bdd_constrain bdd enum-bdds)
	  bdd)
      (let* ((excl-bdd (make-enum-exclusive-bdd
			(mapcar #'(lambda (e) (bdd_create_var (cdr e)))
			  (cdar rec-form-alist))))
	     (incl-bdd (make-enum-inclusive-bdd (car rec-form-alist)))
	     (enum-bdd (if excl-bdd
			   (if incl-bdd
			       (bdd-and excl-bdd incl-bdd)
			       excl-bdd)
			   incl-bdd)))
	(add-enum-bdds
	 bdd
	 (cdr rec-form-alist)
	 (if enum-bdd
	     (if enum-bdds
		 (bdd-and enum-bdds enum-bdd)
		 enum-bdd)
	     enum-bdds)))))

(defun make-enum-exclusive-bdd (enum-list &optional bdd)
  (if (null (cdr enum-list))
      bdd
      (let ((bdd-disj (bdd-or (bdd-not (car enum-list))
			      (bdd-not (bdd-or* (cdr enum-list))))))
	(make-enum-exclusive-bdd
	 (cdr enum-list)
	 (if bdd
	     (bdd-and bdd bdd-disj)
	     bdd-disj)))))

(defun make-enum-inclusive-bdd (rec-alist)
  (let* ((type (find-supertype (type (car rec-alist))))
	 (enum-size (typecase type
		      (adt-type-name (length (constructors (adt type))))
		      (cotupletype (length (types type)))
		      (t (error "bad rec-alist element")))))
    (when (= enum-size (length (cdr rec-alist)))
      (bdd-or* (mapcar #'(lambda (e) (bdd_create_var (cdr e)))
		 (cdr rec-alist))))))

(defmethod translate-to-bdd (expr)
  (translate-to-bdd* expr))

(defmethod translate-to-bdd* ((expr expr))
  (cond ((tc-eq expr *true*) (bdd_1))
	((tc-eq expr *false*) (bdd_0))
	(t (make-bdd-var expr))))

(defmethod translate-to-bdd* ((expr application))
  (cond ((disjunction? expr)
	 (bdd-or (translate-to-bdd* (args1 expr))
		 (translate-to-bdd* (args2 expr))))
	((implication? expr)
	 (bdd-implies (translate-to-bdd* (args1 expr))
		      (translate-to-bdd* (args2 expr))))
	((conjunction? expr)
	 (bdd-and (translate-to-bdd* (args1 expr))
		  (translate-to-bdd* (args2 expr))))
	((iff-or-boolean-equation? expr)
	 (if (or (not *ignore-boolean-equalities?*)
		 (iff? expr))
	     (bdd-equiv (translate-to-bdd* (args1 expr))
			(translate-to-bdd* (args2 expr)))
	     (make-bdd-var expr)))
	((and (disequation? expr)
	      (compatible? (type (args1 expr)) *boolean*))
	 (bdd-not (bdd-equiv (translate-to-bdd* (args1 expr))
			     (translate-to-bdd* (args2 expr)))))
	((negation? expr)
	 (bdd-not (translate-to-bdd* (args1 expr))))
	((branch? expr)
	 (let ((bdd1 (translate-to-bdd* (condition expr)))
	       (bdd2 (translate-to-bdd* (then-part expr)))
	       (bdd3 (translate-to-bdd* (else-part expr))))
	   (bdd_ite bdd1 bdd2 bdd3)))
	(t (make-bdd-var expr))))

(defun make-bdd-var (expr)
  ;;expr should be normalized otherwise a?(x) and x = a get different variables
  ;;and exclusivity asserts NOT(a?(x) & x = a) which is unsound.
  (let* ((rec-appln? (recognizer-application? expr)) ;;
	 (expr (if rec-appln? rec-appln? expr))	;;normalized.
	 (varid (gethash expr *pvs-bdd-hash*)))
    (cond ((null varid)
	   (let ((new-varid (funcall *bdd-counter*)))
	     (setf (gethash expr *pvs-bdd-hash*)
		   new-varid)
	     (setf (gethash new-varid *bdd-pvs-hash*)
		   expr)
	     (enter-into-recognizer-form-alist expr new-varid)
	     (bdd_create_var new-varid)))
	  (t (enter-into-recognizer-form-alist expr varid)
	     (bdd_create_var varid)))))

(defun unit-constructor? (expr)
  (and (constructor? expr)
       (null (accessors expr))))

(defmethod recognizer-application? ((expr application))
  (when (recognizer? (operator expr))
    expr))

;; Note that this doesn't work for cotuples, as they are never
;; unit-constructors.
(defmethod recognizer-application? ((expr equation))
  (if (unit-constructor? (args1 expr))
      (make-application (recognizer (args1 expr)) (args2 expr))
      (if (unit-constructor? (args2 expr))
	  (make-application (recognizer (args2 expr)) (args1 expr))
	  nil)))

(defmethod recognizer-application? ((expr injection?-application))
  expr)

(defmethod recognizer-application? (expr)
  nil)

(defun enter-into-recognizer-form-alist (expr name)
  (let ((recexpr (recognizer-application? expr)))
    (when (not (null recexpr))
      (let* ((op (if (injection?-application? expr)
		     (id expr)
		     (operator recexpr)))
	     (arg (args1 recexpr))
	     (entry (assoc arg *recognizer-forms-alist*
			   :test #'tc-eq)))
	(if (null entry)
	    (push (cons arg (list (cons op name))) *recognizer-forms-alist*)
	    (pushnew (cons op name) (cdr entry) :test #'eql :key #'cdr))))))

(defun translate-from-bdd-list (bddlist)
  (let ((bdds (unless (null-list? bddlist)
		(translate-from-bdd-list* (list_first bddlist)))))
    (mapcar #'translate-bdd-cube bdds)))

(defun translate-from-bdd-list* (bddlist &optional result)
  (if (null-list? bddlist)
      (nreverse result)
      (translate-from-bdd-list*
       (list_next bddlist)
       (cons (elem_contents bddlist) result))))

(defun translate-bdd-cube (cube)
  (cond ((or (bdd-void? cube)
	     (bdd-x? cube))
	 nil)
	((bdd-term? cube)
	 (list *true*))
	(t (translate-bdd-cube* cube))))

(defun translate-bdd-cube* (cube &optional result)
  (if (bdd-term? cube)
      (nreverse result)
      (let ((bdd-t (bdd_cofactor_pos_ cube))
	    (bdd-e (bdd_cofactor_neg_ cube))
	    (varid (bdd_varid cube)))
	(cond ((bdd-0? bdd-e)
	       (translate-bdd-cube* bdd-t (cons varid result)))
	      ((bdd-0? bdd-t)
	       (translate-bdd-cube* bdd-e (cons (list varid) result)))
	      (t (translate-bdd-cube* bdd-t (cons varid result)))))))

(defun bdd-and* (bdd-list &optional bdd)
  (if (null bdd-list)
      (or bdd (bdd_1))
      (bdd-and* (cdr bdd-list)
		(if bdd
		    (bdd_and bdd (car bdd-list))
		    (car bdd-list)))))

(defun bdd-or* (bdd-list &optional bdd)
  (if (null bdd-list)
      (or bdd (bdd_0))
      (bdd-or* (cdr bdd-list)
	       (if bdd
		   (bdd_or bdd (car bdd-list))
		   (car bdd-list)))))
