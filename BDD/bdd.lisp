;;   Copyright 2002 SRI International
(in-package :pvs)
(eval-when (compile)
  (require :foreign))

(eval-when (eval compile load)
;;; List accessors
;;; Lists in the BDD package involve two structures.

;;; A LIST is a structure with slots for a first element pointer, a last
;;; element pointer, the size, and user-defined info.

;;; A LIST_ELEM is a structure with slots for the contents and the next
;;; element.

;;; #define NULL_LIST ((LIST) 0)

;;; void *elem_contents (LIST_ELEM_PTR x)
(ff:def-foreign-call (elem_contents "bdd___elem_contents")
    ((x :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; LIST_ELEM_PTR list_first (LIST x)
(ff:def-foreign-call (list_first "bdd___list_first")
    ((x :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; LIST_ELEM_PTR list_last (LIST x)
(ff:def-foreign-call (list_last "bdd___list_last")
    ((x :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; LIST_INFO list_info (LIST x)
(ff:def-foreign-call (list_info "bdd___list_info")
    ((x :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; LIST_ELEM_PTR list_next (LIST_ELEM_PTR x)
(ff:def-foreign-call (list_next "bdd___list_next")
    ((x :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)

;;; This pretty much follows the bdd.doc sections.

;;; User settable program parameters
;;; --------------------------------
;;; int bdd_do_gc;	            /* default 1 */
;;; set_bdd_do_gc (int flag)
(ff:def-foreign-call (set_bdd_do_gc "bdd___set_bdd_do_gc")
    ((flag :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :void)
;;; int bdd_do_dynamic_ordering;/* default 1 */
;;; set_bdd_do_dynamic_ordering (int flag)
(ff:def-foreign-call (set_bdd_do_dynamic_ordering
		      "bdd___set_bdd_do_dynamic_ordering")
    ((flag :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :void)
;;; int bdd_verbose;            /* default 0 */
;;; set_bdd_verbose (int flag)
(ff:def-foreign-call (set_bdd_verbose "bdd___set_bdd_verbose")
    ((flag :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :void)
;;; int bdd_use_neg_edges;      /* default 1*/
;;; set_bdd_use_neg_edges (int flag)
(ff:def-foreign-call (set_bdd_use_neg_edges "bdd___set_bdd_use_neg_edges")
    ((flag :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; int bdd_use_inv_edges;  /* default 1; 0 when bdd_do_dynamic_ordering = 1 */
;;; set_bdd_use_inv_edges (int flag)
(ff:def-foreign-call (set_bdd_use_inv_edges "bdd___set_bdd_use_inv_edges")
    ((flag :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; int bdd_sizeof_user_data;   /* default 0 */
;;; int BDD_COMPUTED_TABLE_SIZE;/* default DEFAULT_BDD_COMPUTED_TABLE_SIZE */
;;; int BDD_HASHTAB_SIZE;	    /* default DEFAULT_BDD_HASHTAB_SIZE */
;;; int BDD_NR_RANKS;	    /* default DEFAULT_BDD_NR_RANKS */
;;; int BDD_LOAD_FACTOR;        /* default DEFAULT_BDD_LOAD_FACTOR */


;;; C preprocessor macros:
;;; ----------------------

;;; Access to fields of BDD struct:

;;; BDD_VARID (F)
;;; bdd_varid (BDDPTR f)
(ff:def-foreign-call (bdd_varid "bdd___bdd_varid")
    ((f :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDD_THEN (F)
;;; BDD_ELSE (F)
;;; BDD_REFCOUNT (F)
;;; BDD_FLAG (F)
;;; BDD_MARK (F)


;;; Test on terminal nodes:
;;; -----------------------

;;; BDD_VOID_P (f)
;;; int bdd_void_p (BDDPTR f)
(ff:def-foreign-call (bdd_void_p "bdd___bdd_void_p")
    ((f :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDD_1_P (f)
;;; bdd_1_p (BDDPTR f)
(ff:def-foreign-call (bdd_1_p "bdd___bdd_1_p")
    ((f :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDD_0_P (f)
;;; bdd_0_p (BDDPTR f)
(ff:def-foreign-call (bdd_0_p "bdd___bdd_0_p")
    ((f :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDD_X_P (f)
;;; bdd_x_p (BDDPTR f)
(ff:def-foreign-call (bdd_x_p "bdd___bdd_x_p")
    ((f :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDD_CONST_P (f)
;;; int bdd_const_p (BDDPTR f)
(ff:def-foreign-call (bdd_const_p "bdd___bdd_const_p")
    ((f :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDD_TERM_P (f)
;;; bdd_term_p (BDDPTR f)
(ff:def-foreign-call (bdd_term_p "bdd___bdd_term_p")
    ((f :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDD_LIT_P (f)
;;; bdd_lit_p (BDDPTR f)
(ff:def-foreign-call (bdd_lit_p "bdd___bdd_lit_p")
    ((f :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDD_POSLIT_P (f)
;;; int bdd_poslit_p (BDDPTR f)
(ff:def-foreign-call (bdd_poslit_p "bdd___bdd_poslit_p")
    ((f :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDD_NEGLIT_P (f)
;;; int bdd_neglit_p (BDDPTR f)
(ff:def-foreign-call (bdd_neglit_p "bdd___bdd_neglit_p")
    ((f :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDD_COFACTOR_POS (f)
;;; BDDPTR bdd_cofactor_pos_ (BDDPTR f)
(ff:def-foreign-call (bdd_cofactor_pos_ "bdd___bdd_cofactor_pos_")
    ((f :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDD_COFACTOR_NEG (f)
;;; BDDPTR bdd_cofactor_neg_
(ff:def-foreign-call (bdd_cofactor_neg_ "bdd___bdd_cofactor_neg_")
    ((f :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)

;;; void bdd_reset_marks (BDDPTR f)
;;; void bdd_traverse_pre (register BDDPTR v, void (*pre_action)(BDDPTR))
;;; void bdd_traverse_post (register BDDPTR v, void (*post_action)(BDDPTR))

;;; int bdd_size (BDDPTR f)
(ff:def-foreign-call (bdd_size "bdd___bdd_size")
    ((f :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)

;;; int bdd_size_vec (BDDPTR *f_vec, int size)
;;; int bdd_size_ceil (BDDPTR f, int ceiling)

;;; void bdd_init (void)
(ff:def-foreign-call (bdd_init "bdd___bdd_init")
    (:void)
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :void)
;;; void bdd_free (BDDPTR f)
(ff:def-foreign-call (bdd_free "bdd___bdd_free")
    ((f :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :void)
;;; int bdd_gc (void)
(ff:def-foreign-call (bdd_gc "bdd___bdd_gc")
    (:void)
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_ite (BDDPTR F, BDDPTR G, BDDPTR H)
(ff:def-foreign-call (bdd_ite "bdd___bdd_ite")
    ((f :unsigned-int integer)
     (g :unsigned-int integer)
     (h :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_ite_const (BDDPTR F, BDDPTR G, BDDPTR H)
(ff:def-foreign-call (bdd_ite_const "bdd___bdd_ite_const")
    ((f :unsigned-int integer)
     (g :unsigned-int integer)
     (h :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; void bdd_cofactors (BDDPTR f, BDDPTR *vp, BDDPTR *Tp, BDDPTR *Ep)
;;; BDDPTR bdd_invert_input_top (BDDPTR f)
(ff:def-foreign-call (bdd_invert_input_top "bdd___bdd_invert_input_top")
    ((f :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_create_var (int v)
(ff:def-foreign-call (bdd_create_var "bdd___bdd_create_var")
    ((v :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_create_var_first	(void)
(ff:def-foreign-call (bdd_create_var_first "bdd___bdd_create_var_first")
    (:void)
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_create_var_before (BDDPTR v)
(ff:def-foreign-call (bdd_create_var_before "bdd___bdd_create_var_before")
    ((v :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_create_var_after	(BDDPTR v)
(ff:def-foreign-call (bdd_create_var_after "bdd___bdd_create_var_after")
    ((v :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_create_var_last (void)
(ff:def-foreign-call (bdd_create_var_last "bdd___bdd_create_var_last")
    (:void)
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; void bdd_print (FILE *fp, BDDPTR f, char *s)
(ff:def-foreign-call (bdd_print "bdd___bdd_print")
    ((fp :unsigned-int integer)
     (f :unsigned-int integer)
     (s (* :char) simple-string))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :void)
;;; void bdd_print_stats (FILE *fp)
;;; void bdd_quit (void)
(ff:def-foreign-call (bdd_quit "bdd___bdd_quit")
    (:void)
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :void)
;;; int bdd_memsize (void)
;;; int bdd_memsize_limit (void)
;;; void bdd_set_memsize_limit_and_handler (int limit, void (*handler) (void))
;;; int bdd_nodes_alive (void)
(ff:def-foreign-call (bdd_nodes_alive "bdd___bdd_nodes_alive")
    (:void)
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; int bdd_nodes_allocated (void)
(ff:def-foreign-call (bdd_nodes_allocated "bdd___bdd_nodes_allocated")
    (:void)
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; int bdd_nr_occurs_var (int id)
;;; int bdd_compl_p (BDDPTR f, BDDPTR g)
;;; int bdd_equal_p (BDDPTR F, BDDPTR G)
(ff:def-foreign-call (bdd_equal_p "bdd___bdd_equal_p")
    ((f :unsigned-int integer)
     (g :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; int bdd_implies_taut (BDDPTR F, BDDPTR G)
;;; BDDPTR bdd_not (BDDPTR F)
(ff:def-foreign-call (bdd_not "bdd___bdd_not")
    ((f :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_and (BDDPTR F, BDDPTR G)
(ff:def-foreign-call (bdd_and "bdd___bdd_and")
    ((f :unsigned-int integer)
     (g :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_greater	(BDDPTR F, BDDPTR G)
(ff:def-foreign-call (bdd_greater "bdd___bdd_greater")
    ((f :unsigned-int integer)
     (g :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_less (BDDPTR F, BDDPTR G)
(ff:def-foreign-call (bdd_less "bdd___bdd_less")
    ((f :unsigned-int integer)
     (g :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_xor (BDDPTR F, BDDPTR G)
(ff:def-foreign-call (bdd_xor "bdd___bdd_xor")
    ((f :unsigned-int integer)
     (g :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_or (BDDPTR F, BDDPTR G)
(ff:def-foreign-call (bdd_or "bdd___bdd_or")
    ((f :unsigned-int integer)
     (g :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_nor (BDDPTR F, BDDPTR G)
(ff:def-foreign-call (bdd_nor "bdd___bdd_nor")
    ((f :unsigned-int integer)
     (g :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_equiv (BDDPTR F, BDDPTR G)
(ff:def-foreign-call (bdd_equiv "bdd___bdd_equiv")
    ((f :unsigned-int integer)
     (g :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_xnor (BDDPTR F, BDDPTR G) /* equivalent to bdd_equiv */
(ff:def-foreign-call (bdd_xnor "bdd___bdd_xnor")
    ((f :unsigned-int integer)
     (g :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_implied (BDDPTR F, BDDPTR G)
(ff:def-foreign-call (bdd_implied "bdd___bdd_implied")
    ((f :unsigned-int integer)
     (g :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_implies (BDDPTR F, BDDPTR G)
(ff:def-foreign-call (bdd_implies "bdd___bdd_implies")
    ((f :unsigned-int integer)
     (g :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_nand (BDDPTR F, BDDPTR G)
(ff:def-foreign-call (bdd_nand "bdd___bdd_nand")
    ((f :unsigned-int integer)
     (g :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_0 (void)
(ff:def-foreign-call (bdd_0 "bdd___bdd_0")
    (:void)
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_1 (void)
(ff:def-foreign-call (bdd_1 "bdd___bdd_1")
    (:void)
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_X (void)
(ff:def-foreign-call (bdd_X "bdd___bdd_X")
    (:void)
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_assign (BDDPTR f)
(ff:def-foreign-call (bdd_assign "bdd___bdd_assign")
    ((f :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_top_var (BDDPTR f)
;;; int bdd_top_var_rank (BDDPTR f)
;;; BDDPTR bdd_then (BDDPTR f)
(ff:def-foreign-call (bdd_then "bdd___bdd_then")
    ((f :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_else (BDDPTR f)
(ff:def-foreign-call (bdd_else "bdd___bdd_else")
    ((f :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_apply (BDDPTR (*f)(BDDPTR,BDDPTR),BDDPTR a,BDDPTR b)
(ff:def-foreign-call (bdd_apply "bdd___bdd_apply")
    ((f :unsigned-int integer)
     (a :unsigned-int integer)
     (b :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_constrain (BDDPTR f, BDDPTR c)
(ff:def-foreign-call (bdd_constrain "bdd___bdd_constrain")
    ((f :unsigned-int integer)
     (c :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_top_var (BDDPTR f)
(ff:def-foreign-call (bdd_top_var "bdd___bdd_top_var")
    ((f :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDD_LIST bdd_sum_of_cubes (BDDPTR f, int irredundant)
(ff:def-foreign-call (bdd_sum_of_cubes "bdd___bdd_sum_of_cubes")
    ((f :unsigned-int integer)
     (irredundant :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)

(ff:def-foreign-variable bdd_interrupted)

;;; The following were obtained by looking through mu.c and collecting
;;; functions not mentioned above.

;;; int bdd_reorder_var (int var_id, int target_var_id)
(ff:def-foreign-call (bdd_reorder_var "bdd___bdd_reorder_var")
    ((var_id :unsigned-int integer)
     (target_var_id :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_and_smooth (BDDPTR f, BDDPTR g, BDD_LIST vars)
(ff:def-foreign-call (bdd_and_smooth "bdd___bdd_and_smooth")
    ((f :unsigned-int integer)
     (g :unsigned-int integer)
     (vars :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDD_LIST bdd_rank_order_vars (BDD_LIST vars)
(ff:def-foreign-call (bdd_rank_order_vars "bdd___bdd_rank_order_vars")
    ((vars :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_quantify (int existential, BDDPTR f, BDD_LIST vars)
(ff:def-foreign-call (bdd_quantify "bdd___bdd_quantify")
    ((existential :unsigned-int integer)
     (f :unsigned-int integer)
     (vars :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_subst_par (BDDPTR *f_vec, BDD_LIST vars, BDDPTR g)
(ff:def-foreign-call (bdd_subst_par "bdd___bdd_subst_par")
    ((f_vec (:array :unsigned-int))
     (vars :unsigned-int)
     (g :unsigned-int))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_subst_par_list (BDD_LIST f_list, BDD_LIST vars, BDDPTR g)
(ff:def-foreign-call (bdd_subst_par_list "bdd___bdd_subst_par_list")
    ((f_list :unsigned-int)
     (vars :unsigned-int)
     (g :unsigned-int))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; void bdd_free_vec (BDDPTR *f_vec, int size)
(ff:def-foreign-call (bdd_free_vec "bdd___bdd_free_vec")
    ((f_vec :unsigned-int integer)
     (size :int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :void)
;;; const char *bdd_get_output_string (int idx)
(ff:def-foreign-call (bdd_get_output_string "bdd___bdd_get_output_string")
    ((idx :int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; void bdd_set_output_string (int idx, const char *str)
(ff:def-foreign-call (bdd_set_output_string "bdd___bdd_set_output_string")
    ((idx :int integer)
     (str (* :char) simple-string))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :void)
;;; void bdd_print_as_sum_of_cubes (FILE *fp, BDDPTR f, int irredundant)
(ff:def-foreign-call (bdd_print_as_sum_of_cubes
		      "bdd___bdd_print_as_sum_of_cubes")
    ((fp :unsigned-int integer)
     (f :unsigned-int integer)
     (irredundant :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :void)
;;; BDDPTR bdd_diff (BDDPTR f, BDD_LIST vars)
(ff:def-foreign-call (bdd_diff "bdd___bdd_diff")
    ((f :unsigned-int integer)
     (vars :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_one_of_vec (BDDPTR *vec, int size)
(ff:def-foreign-call (bdd_one_of_vec "bdd___bdd_one_of_vec")
    ((vec :unsigned-int integer)
     (size :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_none_of_vec (BDDPTR *args, int size)
(ff:def-foreign-call (bdd_none_of_vec "bdd___bdd_none_of_vec")
    ((args :unsigned-int integer)
     (size :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_subst (BDDPTR f, int var, BDDPTR g)
(ff:def-foreign-call (bdd_subst "bdd___bdd_subst")
    ((f :unsigned-int integer)
     (var :unsigned-int integer)
     (g :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDD_LIST bdd_sum_of_cubes_as_list (BDDPTR f)
(ff:def-foreign-call (bdd_sum_of_cubes_as_list
		      "bdd___bdd_sum_of_cubes_as_list")
    ((f :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; int bdd_traverse_cube (BDDPTR cube,
;;;                        void (*action) (int index, int neg, int first))
(ff:def-foreign-call (bdd_traverse_cube "bdd___bdd_traverse_cube")
    ((cube :unsigned-int integer)
     (action :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDD_LIST bdd_support_as_list_of_vars (BDDPTR f)
(ff:def-foreign-call (bdd_support_as_list_of_vars "bdd___bdd_support_as_list_of_vars")
    ((f :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)

)

(eval-when (eval load)
  (bdd_init))

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
	     ;; Need to look at this, or somehow it gets a nonzero value
	     bdd_interrupted
	     (if dynamic-ordering?
		 (set_bdd_do_dynamic_ordering 1)
		 (set_bdd_do_dynamic_ordering 0))
	     (let* ((remaining-sforms (delete-seq sforms fnums))
		    (conjuncts (bddsimp-conjuncts selected-sforms
						  irredundant?)))
	       (cond ((zerop bdd_interrupted)
		      (multiple-value-prog1
		       (add-bdd-subgoals ps sforms conjuncts remaining-sforms)
		       (unless *bdd-initialized* (bdd_quit))))
		     (t (format t "~%BDD Simplifier interrupted")
			(values 'X nil))))))))

(defun bddsimp-conjuncts (selected-sforms irredundant?)
  (let* ((*pvs-bdd-hash* (make-hash-table
			  :hash-function 'pvs-sxhash :test 'tc-eq))
	 (*bdd-pvs-hash* (make-hash-table))
	 (*bdd-counter* (let ((x 0)) #'(lambda () (incf x))))
	 (*recognizer-forms-alist* nil)
	 (sforms-bdd (make-sforms-bdd selected-sforms))
	 (list-of-conjuncts (translate-from-bdd-list
			     (bdd_sum_of_cubes sforms-bdd
					       (if irredundant? 1 0)))))
       (from-bdd-list-to-pvs-list list-of-conjuncts))
)


(defvar *ignore-boolean-equalities?* nil)

(defun bdd-simplify (expr &optional ignore-boolean-equalities?)
  (unless *bdd-initialized* (bdd_init))
  (let* ((*pvs-bdd-hash* (make-hash-table
			  :hash-function 'pvs-sxhash :test 'tc-eq))
	 (*bdd-pvs-hash* (make-hash-table))
	 (*recognizer-forms-alist* nil)
	 (*ignore-boolean-equalities?* ignore-boolean-equalities?)
	 (bdd (bdd_not (translate-to-bdd expr)))
	 (ebdd (add-enum-bdds bdd *recognizer-forms-alist*))
	 (bdd-list (translate-from-bdd-list (bdd_sum_of_cubes ebdd 1)))
	 (pvs-list (from-bdd-list-to-pvs-list (nreverse bdd-list))))
    (cond ((zerop bdd_interrupted)
	   (prog1 (make!-conjunction*
		   (mapcar #'make!-disjunction* pvs-list))
	     (unless *bdd-initialized* (bdd_quit))))
	  (t (format t "~%BDD Simplifier interrupted")
	     expr))))

(defun from-bdd-list-to-pvs-list (list-of-conjuncts)
   (init-hash-tables) ;; definition in mu.lisp
  (let* ((lit-list (mapcar #'(lambda (conj)
			     (mapcar #'(lambda (lit)
				         (if (consp lit)
					     (gethash (car lit) *bdd-pvs-hash*)
					     (make-negation
				      	(gethash lit *bdd-pvs-hash*))))
		      	 conj))
		   list-of-conjuncts)))
  (assert (hash-table-p *pvs-bdd-hash*))
    lit-list)
)


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
  (let ((enum-size (length (constructors
			    (adt (find-supertype (type (car rec-alist))))))))
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
	 (expr (if rec-appln? rec-appln? expr))      ;;normalized.
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

(defun recognizer-application? (expr)
  (if (and (application? expr)
	   (recognizer? (operator expr)))
      expr
      (if (equation? expr)
	  (if (unit-constructor? (args1 expr))
	      (make-application (recognizer (args1 expr)) (args2 expr))
	      (if (unit-constructor? (args2 expr))
		  (make-application (recognizer (args2 expr)) (args1 expr))
		  nil))
	  nil)))

(defun enter-into-recognizer-form-alist (expr name)
  (let ((recexpr (recognizer-application? expr)))
    (when (not (null recexpr))
      (let* ((op (operator recexpr))
	     (arg (args1 recexpr))
	     (entry (assoc  arg *recognizer-forms-alist*
			    :test #'tc-eq)))
	(if (null entry)
	    (push (cons arg (list (cons op name))) *recognizer-forms-alist*)
	    (pushnew (cons op name) (cdr entry) :test #'eql :key #'cdr))))))

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
