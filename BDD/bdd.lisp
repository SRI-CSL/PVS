(in-package 'pvs)

(require :foreign)

;;; List accessors
;;; Lists in the BDD package involve two structures.

;;; A LIST is a structure with slots for a first element pointer, a last
;;; element pointer, the size, and user-defined info.

;;; A LIST_ELEM is a structure with slots for the contents and the next
;;; element.

;;; #define NULL_LIST ((LIST) 0)
(ff:defforeign 'null_list)
;;; #define ELEM_CONTENTS(elem) ((elem)->cont)
(ff:defforeign 'elem_contents)
;;; #define LIST_FIRST(list) ((list)->start_p)
(ff:defforeign 'list_first)
;;; #define LIST_LAST(list)  ((list)->end_p)
(ff:defforeign 'list_last)
;;; #define LIST_INFO(list)  ((list)->info)
(ff:defforeign 'list_info)
;;; #define LIST_NEXT(elem)  ((elem)->next)
(ff:defforeign 'list_next)

;;; This pretty much follows the bdd.doc sections.

;;; User settable program parameters
;;; --------------------------------
;;; int bdd_do_gc;	            /* default 1 */
(ff:defforeign 'set_bdd_do_gc)
;;; int bdd_do_dynamic_ordering;/* default 1 */
(ff:defforeign 'set_bdd_do_dynamic_ordering)
;;; int bdd_verbose;            /* default 0 */
(ff:defforeign 'set_bdd_verbose)
;;; int bdd_use_neg_edges;      /* default 1*/
(ff:defforeign 'set_bdd_use_neg_edges)
;;; int bdd_use_inv_edges;      /* default 1; 0 when bdd_do_dynamic_ordering = 1 */
(ff:defforeign 'set_bdd_use_inv_edges)
;;; int bdd_sizeof_user_data;   /* default 0 */
;;; int BDD_COMPUTED_TABLE_SIZE;/* default DEFAULT_BDD_COMPUTED_TABLE_SIZE */
;;; int BDD_HASHTAB_SIZE;	    /* default DEFAULT_BDD_HASHTAB_SIZE */
;;; int BDD_NR_RANKS;	    /* default DEFAULT_BDD_NR_RANKS */
;;; int BDD_LOAD_FACTOR;        /* default DEFAULT_BDD_LOAD_FACTOR */


;;; C preprocessor macros:
;;; ----------------------

;;; Access to fields of BDD struct:

;;; BDD_VARID (F)
(ff:defforeign 'bdd_varid)
;;; BDD_THEN (F)
;;; BDD_ELSE (F)
;;; BDD_REFCOUNT (F)
;;; BDD_FLAG (F)
;;; BDD_MARK (F)


;;; Test on terminal nodes:
;;; -----------------------

;;; BDD_VOID_P (f)
(ff:defforeign 'bdd_void_p)
;;; BDD_1_P (f)
(ff:defforeign 'bdd_1_p)
;;; BDD_0_P (f)
(ff:defforeign 'bdd_0_p)
;;; BDD_X_P (f)
(ff:defforeign 'bdd_x_p)
;;; BDD_CONST_P (f)
(ff:defforeign 'bdd_const_p)
;;; BDD_TERM_P (f)
(ff:defforeign 'bdd_term_p)
;;; BDD_LIT_P (f)
(ff:defforeign 'bdd_lit_p)
;;; BDD_POSVAR_P (f)
;;; BDD_NEGVAR_P (f)
;;; BDD_COFACTOR_POS (f)
(ff:defforeign 'bdd_cofactor_pos_)
;;; BDD_COFACTOR_NEG (f)
(ff:defforeign 'bdd_cofactor_neg_)

;;; void bdd_reset_marks (BDDPTR f)
;;; void bdd_traverse_pre (register BDDPTR v, void (*pre_action)(BDDPTR))
;;; void bdd_traverse_post (register BDDPTR v, void (*post_action)(BDDPTR))

;;; int bdd_size (BDDPTR f)
(ff:defforeign 'bdd_size)

;;; int bdd_size_vec (BDDPTR *f_vec, int size)
;;; int bdd_size_ceil (BDDPTR f, int ceiling)

;;; void bdd_init (void)
(ff:defforeign 'bdd_init)
;;; void bdd_free (BDDPTR f)
(ff:defforeign 'bdd_free)
;;; int bdd_gc (void)
(ff:defforeign 'bdd_gc)
;;; BDDPTR bdd_ite (BDDPTR F, BDDPTR G, BDDPTR H)
(ff:defforeign 'bdd_ite)
;;; BDDPTR bdd_ite_const (BDDPTR F, BDDPTR G, BDDPTR H)
(ff:defforeign 'bdd_ite_const)
;;; void bdd_cofactors (BDDPTR f, BDDPTR *vp, BDDPTR *Tp, BDDPTR *Ep)
;;; BDDPTR bdd_invert_input_top (BDDPTR f)
(ff:defforeign 'bdd_invert_input_top)
;;; BDDPTR bdd_create_var (int v)
(ff:defforeign 'bdd_create_var)
;;; BDDPTR bdd_create_var_first	(void)
(ff:defforeign 'bdd_create_var_first)

(ff:defforeign 'bdd_create_var_before)
;;; BDDPTR bdd_create_var_after	(BDDPTR v)
(ff:defforeign 'bdd_create_var_after)
;;; BDDPTR bdd_create_var_last	(void)
(ff:defforeign 'bdd_create_var_last)
;;; void bdd_print (FILE *fp, BDDPTR f, char *s)
(ff:defforeign 'bdd_print)
;;; void bdd_print_stats (FILE *fp)
;;; void bdd_quit (void)
(ff:defforeign 'bdd_quit)
;;; int bdd_memsize (void)
;;; int bdd_memsize_limit (void)
;;; void bdd_set_memsize_limit_and_handler (int limit, void (*handler) (void))
;;; int bdd_nodes_alive (void)
(ff:defforeign 'bdd_nodes_alive)
;;; int bdd_nodes_allocated (void)
(ff:defforeign 'bdd_nodes_allocated)
;;; int bdd_nr_occurs_var (int id)
;;; int bdd_compl_p (BDDPTR f, BDDPTR g)
;;; int bdd_equal_p (BDDPTR F, BDDPTR G)
;;; int bdd_implies_taut (BDDPTR F, BDDPTR G)
;;; BDDPTR bdd_not (BDDPTR F)
(ff:defforeign 'bdd_not)
;;; BDDPTR bdd_and (BDDPTR F, BDDPTR G)
(ff:defforeign 'bdd_and)
;;; BDDPTR bdd_greater	(BDDPTR F, BDDPTR G)
(ff:defforeign 'bdd_greater)
;;; BDDPTR bdd_less (BDDPTR F, BDDPTR G)
(ff:defforeign 'bdd_less)
;;; BDDPTR bdd_xor (BDDPTR F, BDDPTR G)
(ff:defforeign 'bdd_xor)
;;; BDDPTR bdd_or (BDDPTR F, BDDPTR G)
(ff:defforeign 'bdd_or)
;;; BDDPTR bdd_nor (BDDPTR F, BDDPTR G)
(ff:defforeign 'bdd_nor)
;;; BDDPTR bdd_equiv (BDDPTR F, BDDPTR G)
(ff:defforeign 'bdd_equiv)
;;; BDDPTR bdd_xnor (BDDPTR F, BDDPTR G) /* equivalent to bdd_equiv */
(ff:defforeign 'bdd_xnor)
;;; BDDPTR bdd_implied (BDDPTR F, BDDPTR G)
(ff:defforeign 'bdd_implied)
;;; BDDPTR bdd_implies (BDDPTR F, BDDPTR G)
(ff:defforeign 'bdd_implies)
;;; BDDPTR bdd_nand (BDDPTR F, BDDPTR G)
(ff:defforeign 'bdd_nand)
;;; BDDPTR bdd_0 (void)
(ff:defforeign 'bdd_0)
;;; BDDPTR bdd_1 (void)
(ff:defforeign 'bdd_1)
;;; BDDPTR bdd_X (void)
(ff:defforeign 'bdd_X)
;;; BDDPTR bdd_assign (BDDPTR f)
(ff:defforeign 'bdd_assign)
;;; BDDPTR bdd_top_var (BDDPTR f)
;;; int bdd_top_var_rank (BDDPTR f)
;;; BDDPTR bdd_then (BDDPTR f)
(ff:defforeign 'bdd_then)
;;; BDDPTR bdd_else (BDDPTR f)
(ff:defforeign 'bdd_else)
;;; BDDPTR bdd_apply (BDDPTR (*f)(BDDPTR,BDDPTR),BDDPTR a,BDDPTR b)

(ff:defforeign 'bdd_sum_of_cubes)

(eval-when (eval load)
  (bdd_init))

(defmacro bdd-void? (bdd) `(= (bdd_void_p ,bdd) 1))
(defmacro bdd-1? (bdd) `(= (bdd_1_p ,bdd) 1))
(defmacro bdd-0? (bdd) `(= (bdd_0_p ,bdd) 1))
(defmacro bdd-x? (bdd) `(= (bdd_x_p ,bdd) 1))
(defmacro bdd-term? (bdd) `(= (bdd_term_p ,bdd) 1))

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


(defvar *pvs-bdd-hash* nil)
(defvar *bdd-pvs-hash* nil)

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
  (bdd_init)
  (if dynamic-ordering?
      (set_bdd_do_dynamic_ordering 1)
      (set_bdd_do_dynamic_ordering 0))
  (let* ((sforms (s-forms (current-goal ps)))
	 (selected-sforms (select-seq sforms fnums))
	 (remaining-sforms (delete-seq sforms fnums))
	 (conjuncts (bddsimp-conjuncts selected-sforms irredundant?)))
    (multiple-value-prog1
     (add-bdd-subgoals ps sforms conjuncts remaining-sforms)
     (bdd_quit))))

(defun bddsimp-conjuncts (selected-sforms irredundant?)
  (let* ((*pvs-bdd-hash* (make-pvs-hash-table
			  :hashfn #'pvs-sxhash :test #'tc-eq))
	 (*bdd-pvs-hash* (make-hash-table))
	 (sforms-bdd (make-sforms-bdd selected-sforms))
	 (list-of-conjuncts (translate-from-bdd-list
			     (bdd_sum_of_cubes sforms-bdd
					       (if irredundant? 1 0))))
	 (lit-list (mapcar #'(lambda (conj)
			       (mapcar #'(lambda (lit)
					   (if (consp lit)
					       (gethash (car lit) *bdd-pvs-hash*)
					       (make-negation
						(gethash lit *bdd-pvs-hash*))))
				 conj))
		     list-of-conjuncts)))
    lit-list))

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
      bdd
      (let* ((fbdd (translate-to-bdd (formula (car selected-sforms))))
	     (negbdd (bdd_not fbdd)))
	(make-sforms-bdd (cdr selected-sforms)
			 (if bdd
			     (bdd_and bdd negbdd)
			     negbdd)))))

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
	((iff? expr)
	 (bdd-equiv (translate-to-bdd* (args1 expr))
		    (translate-to-bdd* (args2 expr))))
	((not-expr? expr)
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
	 (expr (if rec-appln? rec-appln? expr))      ;;normalized.
	 (varid (pvs-gethash expr *pvs-bdd-hash*)))
    (cond ((null varid)
	   (let ((new-varid (funcall *bdd-counter*)))
	       (setf (pvs-gethash expr *pvs-bdd-hash*)
		     new-varid)
	       (setf (gethash new-varid *bdd-pvs-hash*)
		     expr)
	       (enter-into-recognizer-form-alist expr new-varid)
	       (bdd_create_var new-varid)))
	  (t (enter-into-recognizer-form-alist expr varid)
	     (bdd_create_var varid)))))

(defun translate-from-bdd-list (bddlist)
  (let ((bdds (unless (zerop bddlist)
		(translate-from-bdd-list* (list_first bddlist)))))
    (mapcar #'translate-bdd-cube bdds)))

(defun translate-from-bdd-list* (bddlist &optional result)
  (if (zerop bddlist)
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
