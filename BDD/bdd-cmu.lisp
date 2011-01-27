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

(shadow '(INT VOID UNSIGNED-INT))

;; (use-package :alien)
(use-package :c-call)

;; SO - We load this from pvs-init (in src/pvs.lisp), as it requires mu.so,
;; but if mu.so is loaded at save-lisp time, it doesn't work (at least I
;; can't get it to).

;;; List accessors
;;; Lists in the BDD package involve two structures.

;;; A LIST is a structure with slots for a first element pointer, a last
;;; element pointer, the size, and user-defined info.

;;; A LIST_ELEM is a structure with slots for the contents and the next
;;; element.

;;; #define NULL_LIST ((LIST) 0)

;;; int null_list_p (LIST x)
(alien:def-alien-routine ("bdd___null_list_p" null_list_p)
			 unsigned-int
  (x unsigned-int))
    
;;; void *elem_contents (LIST_ELEM_PTR x)
(alien:def-alien-routine ("bdd___elem_contents" elem_contents)
			 unsigned-int
  (x unsigned-int))
    
;;; LIST_ELEM_PTR list_first (LIST x)
(alien:def-alien-routine ("bdd___list_first" list_first)
			 unsigned-int
  (x unsigned-int))

;;; LIST_ELEM_PTR list_last (LIST x)
(alien:def-alien-routine ("bdd___list_last" list_last)
			 unsigned-int
  (x unsigned-int))

;;; LIST_INFO list_info (LIST x)
(alien:def-alien-routine ("bdd___list_info" list_info)
			 unsigned-int
  (x unsigned-int))
    
;;; LIST_ELEM_PTR list_next (LIST_ELEM_PTR x)
(alien:def-alien-routine ("bdd___list_next" list_next)
			 unsigned-int
  (x unsigned-int))

;;; This pretty much follows the bdd.doc sections.

;;; User settable program parameters
;;; --------------------------------
;;; int bdd_do_gc;	            /* default 1 */
;;; set_bdd_do_gc (int flag)
(alien:def-alien-routine ("bdd___set_bdd_do_gc" set_bdd_do_gc)
			 void
  (flag unsigned-int))

;;; int bdd_do_dynamic_ordering;/* default 1 */
;;; set_bdd_do_dynamic_ordering (int flag)
(alien:def-alien-routine ("bdd___set_bdd_do_dynamic_ordering" set_bdd_do_dynamic_ordering)
			 void
  (flag unsigned-int))
    
;;; int bdd_verbose;            /* default 0 */
;;; set_bdd_verbose (int flag)
(alien:def-alien-routine ("bdd___set_bdd_verbose" set_bdd_verbose)
			 void
  (flag unsigned-int))

;;; int bdd_use_neg_edges;      /* default 1*/
;;; set_bdd_use_neg_edges (int flag)
(alien:def-alien-routine ("bdd___set_bdd_use_neg_edges" set_bdd_use_neg_edges)
			 unsigned-int
  (flag unsigned-int))

;;; int bdd_use_inv_edges;  /* default 1; 0 when bdd_do_dynamic_ordering = 1 */
;;; set_bdd_use_inv_edges (int flag)
(alien:def-alien-routine ("bdd___set_bdd_use_inv_edges" set_bdd_use_inv_edges)
			 unsigned-int
  (flag unsigned-int))

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
(alien:def-alien-routine ("bdd___bdd_varid" bdd_varid)
			 unsigned-int
  (f unsigned-int))

;;; BDD_THEN (F)
;;; BDD_ELSE (F)
;;; BDD_REFCOUNT (F)
;;; BDD_FLAG (F)
;;; BDD_MARK (F)


;;; Test on terminal nodes:
;;; -----------------------

;;; BDD_VOID_P (f)
;;; int bdd_void_p (BDDPTR f)
(alien:def-alien-routine ("bdd___bdd_void_p" bdd_void_p)
			 unsigned-int
  (f unsigned-int))

;;; BDD_1_P (f)
;;; bdd_1_p (BDDPTR f)
(alien:def-alien-routine ("bdd___bdd_1_p" bdd_1_p)
			 unsigned-int
  (f unsigned-int))

;;; BDD_0_P (f)
;;; bdd_0_p (BDDPTR f)
(alien:def-alien-routine ("bdd___bdd_0_p" bdd_0_p)
			 unsigned-int
  (f unsigned-int))

;;; BDD_X_P (f)
;;; bdd_x_p (BDDPTR f)
(alien:def-alien-routine ("bdd___bdd_x_p" bdd_x_p)
			 unsigned-int
  (f unsigned-int))

;;; BDD_CONST_P (f)
;;; int bdd_const_p (BDDPTR f)
(alien:def-alien-routine ("bdd___bdd_const_p" bdd_const_p)
			 unsigned-int
  (f unsigned-int))

;;; BDD_TERM_P (f)
;;; bdd_term_p (BDDPTR f)
(alien:def-alien-routine ("bdd___bdd_term_p" bdd_term_p)
			 unsigned-int
  (f unsigned-int))

;;; BDD_LIT_P (f)
;;; bdd_lit_p (BDDPTR f)
(alien:def-alien-routine ("bdd___bdd_lit_p" bdd_lit_p)
			 unsigned-int
  (f unsigned-int))

;;; BDD_POSLIT_P (f)
;;; int bdd_poslit_p (BDDPTR f)
(alien:def-alien-routine ("bdd___bdd_poslit_p" bdd_poslit_p)
			 unsigned-int
  (f unsigned-int))

;;; BDD_NEGLIT_P (f)
;;; int bdd_neglit_p (BDDPTR f)
(alien:def-alien-routine ("bdd___bdd_neglit_p" bdd_neglit_p)
			 unsigned-int
  (f unsigned-int))

;;; BDD_COFACTOR_POS (f)
;;; BDDPTR bdd_cofactor_pos_ (BDDPTR f)
(alien:def-alien-routine ("bdd___bdd_cofactor_pos_" bdd_cofactor_pos_)
			 unsigned-int
  (f unsigned-int))

;;; BDD_COFACTOR_NEG (f)
;;; BDDPTR bdd_cofactor_neg_
(alien:def-alien-routine ("bdd___bdd_cofactor_neg_" bdd_cofactor_neg_)
			 unsigned-int
  (f unsigned-int))

;;; void bdd_reset_marks (BDDPTR f)
;;; void bdd_traverse_pre (register BDDPTR v, void (*pre_action)(BDDPTR))
;;; void bdd_traverse_post (register BDDPTR v, void (*post_action)(BDDPTR))

;;; int bdd_size (BDDPTR f)
(alien:def-alien-routine ("bdd___BDD_bdd_size" BDD_bdd_size)
			 unsigned-int
  (f unsigned-int))

;;; int bdd_size_vec (BDDPTR *f_vec, int size)
;;; int bdd_size_ceil (BDDPTR f, int ceiling)

;;; void bdd_init (void)
(alien:def-alien-routine ("bdd___BDD_bdd_init" BDD_bdd_init)
			 void)

;;; void bdd_free (BDDPTR f)
;; (alien:def-alien-routine ("bdd___bdd_free" bdd_free) void
;;   (f unsigned-int))

;; SO - for some reason, bdd_free causes problems.  Since we call bdd_init
;; each time anyway, this is only a problem for large BDDs, components of
;; which can't be freed during construction.
(defun bdd_free (ff) nil)

;;; int bdd_gc (void)
(alien:def-alien-routine ("bdd___bdd_gc" bdd_gc)
			 unsigned-int)

;;; BDDPTR bdd_ite (BDDPTR F, BDDPTR G, BDDPTR H)
(alien:def-alien-routine ("bdd___bdd_ite" bdd_ite)
			 unsigned-int
  (f unsigned-int)
  (g unsigned-int)
  (h unsigned-int))

;;; BDDPTR bdd_ite_const (BDDPTR F, BDDPTR G, BDDPTR H)
(alien:def-alien-routine ("bdd___bdd_ite_const" bdd_ite_const)
			 unsigned-int
  (f unsigned-int)
  (g unsigned-int)
  (h unsigned-int))

;;; void bdd_cofactors (BDDPTR f, BDDPTR *vp, BDDPTR *Tp, BDDPTR *Ep)
;;; BDDPTR bdd_invert_input_top (BDDPTR f)
(alien:def-alien-routine ("bdd___bdd_invert_input_top" bdd_invert_input_top)
			 unsigned-int
  (f unsigned-int))

;;; BDDPTR bdd_create_var (int v)
(alien:def-alien-routine ("bdd___bdd_create_var" bdd_create_var)
			 unsigned-int
  (v unsigned-int))

;;; BDDPTR bdd_create_var_first	(void)
(alien:def-alien-routine ("bdd___bdd_create_var_first" bdd_create_var_first)
			 unsigned-int
  )

;;; BDDPTR bdd_create_var_before (BDDPTR v)
(alien:def-alien-routine ("bdd___bdd_create_var_before" bdd_create_var_before)
			 unsigned-int
  (v unsigned-int))

;;; BDDPTR bdd_create_var_after	(BDDPTR v)
(alien:def-alien-routine ("bdd___bdd_create_var_after" bdd_create_var_after)
			 unsigned-int
  (v unsigned-int))

;;; BDDPTR bdd_create_var_last (void)
(alien:def-alien-routine ("bdd___bdd_create_var_last" bdd_create_var_last)
			 unsigned-int
  )

;;; void bdd_print (FILE *fp, BDDPTR f, char *s)
(alien:def-alien-routine ("bdd___bdd_print" bdd_print)
			 void
  (fp unsigned-int)
  (f unsigned-int)
  (s c-string))

;;; void bdd_print_stats (FILE *fp)
;;; void bdd_quit (void)
(alien:def-alien-routine ("bdd___bdd_quit" bdd_quit)
			 void
  )

;;; int bdd_memsize (void)
;;; int bdd_memsize_limit (void)
;;; void bdd_set_memsize_limit_and_handler (int limit, void (*handler) (void))
;;; int bdd_nodes_alive (void)
(alien:def-alien-routine ("bdd___bdd_nodes_alive" bdd_nodes_alive)
			 unsigned-int
  )

;;; int bdd_nodes_allocated (void)
(alien:def-alien-routine ("bdd___bdd_nodes_allocated" bdd_nodes_allocated)
			 unsigned-int
  )

;;; int bdd_nr_occurs_var (int id)
;;; int bdd_compl_p (BDDPTR f, BDDPTR g)
;;; int bdd_equal_p (BDDPTR F, BDDPTR G)
(alien:def-alien-routine ("bdd___bdd_equal_p" bdd_equal_p)
			 unsigned-int
  (f unsigned-int)
  (g unsigned-int))

;;; int bdd_implies_taut (BDDPTR F, BDDPTR G)
;;; BDDPTR bdd_not (BDDPTR F)
(alien:def-alien-routine ("bdd___bdd_not" bdd_not)
			 unsigned-int
  (f unsigned-int))

;;; BDDPTR bdd_and (BDDPTR F, BDDPTR G)
(alien:def-alien-routine ("bdd___bdd_and" bdd_and)
			 unsigned-int
  (f unsigned-int)
  (g unsigned-int))

;;; BDDPTR bdd_greater	(BDDPTR F, BDDPTR G)
(alien:def-alien-routine ("bdd___bdd_greater" bdd_greater)
			 unsigned-int
  (f unsigned-int)
  (g unsigned-int))

;;; BDDPTR bdd_less (BDDPTR F, BDDPTR G)
(alien:def-alien-routine ("bdd___bdd_less" bdd_less)
			 unsigned-int
  (f unsigned-int)
  (g unsigned-int))

;;; BDDPTR bdd_xor (BDDPTR F, BDDPTR G)
(alien:def-alien-routine ("bdd___bdd_xor" bdd_xor)
			 unsigned-int
  (f unsigned-int)
  (g unsigned-int))

;;; BDDPTR bdd_or (BDDPTR F, BDDPTR G)
(alien:def-alien-routine ("bdd___bdd_or" bdd_or)
			 unsigned-int
  (f unsigned-int)
  (g unsigned-int))

;;; BDDPTR bdd_nor (BDDPTR F, BDDPTR G)
(alien:def-alien-routine ("bdd___bdd_nor" bdd_nor)
			 unsigned-int
  (f unsigned-int)
  (g unsigned-int))

;;; BDDPTR bdd_equiv (BDDPTR F, BDDPTR G)
(alien:def-alien-routine ("bdd___bdd_equiv" bdd_equiv)
			 unsigned-int
  (f unsigned-int)
  (g unsigned-int))

;;; BDDPTR bdd_xnor (BDDPTR F, BDDPTR G) /* equivalent to bdd_equiv */
(alien:def-alien-routine ("bdd___bdd_xnor" bdd_xnor)
			 unsigned-int
  (f unsigned-int)
  (g unsigned-int))

;;; BDDPTR bdd_implied (BDDPTR F, BDDPTR G)
(alien:def-alien-routine ("bdd___bdd_implied" bdd_implied)
			 unsigned-int
  (f unsigned-int)
  (g unsigned-int))

;;; BDDPTR bdd_implies (BDDPTR F, BDDPTR G)
(alien:def-alien-routine ("bdd___bdd_implies" bdd_implies)
			 unsigned-int
  (f unsigned-int)
  (g unsigned-int))

;;; BDDPTR bdd_nand (BDDPTR F, BDDPTR G)
(alien:def-alien-routine ("bdd___bdd_nand" bdd_nand)
			 unsigned-int
  (f unsigned-int)
  (g unsigned-int))

;;; BDDPTR bdd_0 (void)
(alien:def-alien-routine ("bdd___bdd_0" bdd_0)
			 unsigned-int
  )

;;; BDDPTR bdd_1 (void)
(alien:def-alien-routine ("bdd___bdd_1" bdd_1)
			 unsigned-int
  )

;;; BDDPTR bdd_X (void)
(alien:def-alien-routine ("bdd___bdd_X" bdd_X)
			 unsigned-int)

;;; BDDPTR bdd_assign (BDDPTR f)
(alien:def-alien-routine ("bdd___bdd_assign" bdd_assign)
			 unsigned-int
  (f unsigned-int))

;;; BDDPTR bdd_top_var (BDDPTR f)
;;; int bdd_top_var_rank (BDDPTR f)
;;; BDDPTR bdd_then (BDDPTR f)
(alien:def-alien-routine ("bdd___BDD_bdd_then" BDD_bdd_then)
			 unsigned-int
  (f unsigned-int))

;;; BDDPTR bdd_else (BDDPTR f)
(alien:def-alien-routine ("bdd___BDD_bdd_else" BDD_bdd_else)
			 unsigned-int
  (f unsigned-int))

;;; BDDPTR bdd_apply (BDDPTR (*f)(BDDPTR,BDDPTR),BDDPTR a,BDDPTR b)
(alien:def-alien-routine ("bdd___bdd_apply" bdd_apply)
			 unsigned-int
  (f unsigned-int)
  (a unsigned-int)
  (b unsigned-int))

;;; BDDPTR bdd_constrain (BDDPTR f, BDDPTR c)
(alien:def-alien-routine ("bdd___bdd_constrain" bdd_constrain)
			 unsigned-int
  (f unsigned-int)
  (c unsigned-int))

;;; BDDPTR bdd_top_var (BDDPTR f)
(alien:def-alien-routine ("bdd___bdd_top_var" bdd_top_var)
			 unsigned-int
  (f unsigned-int))

;;; BDD_LIST bdd_sum_of_cubes (BDDPTR f, int irredundant)
(alien:def-alien-routine ("bdd___bdd_sum_of_cubes" bdd_sum_of_cubes)
			 unsigned-int
  (f unsigned-int)
  (irredundant unsigned-int))

(alien:def-alien-variable bdd_interrupted integer)

;;; The following were obtained by looking through mu.c and collecting
;;; functions not mentioned above.

;;; int bdd_reorder_var (int var_id, int target_var_id)
(alien:def-alien-routine ("bdd___bdd_reorder_var" bdd_reorder_var)
			 unsigned-int
  (var_id unsigned-int)
  (target_var unsigned-int))

;;; BDDPTR bdd_and_smooth (BDDPTR f, BDDPTR g, BDD_LIST vars)
(alien:def-alien-routine ("bdd___bdd_and_smooth" bdd_and_smooth)
			 unsigned-int
  (f unsigned-int)
  (g unsigned-int)
  (vars unsigned-int))

;;; BDD_LIST bdd_rank_order_vars (BDD_LIST vars)
(alien:def-alien-routine ("bdd___bdd_rank_order_vars" bdd_rank_order_vars)
			 unsigned-int
  (vars unsigned-int))

;;; BDDPTR bdd_quantify (int existential, BDDPTR f, BDD_LIST vars)
(alien:def-alien-routine ("bdd___bdd_quantify" bdd_quantify)
			 unsigned-int
  (existential unsigned-int)
  (f unsigned-int)
  (vars unsigned-int))

;;; BDDPTR bdd_subst_par (BDDPTR *f_vec, BDD_LIST vars, BDDPTR g)
(alien:def-alien-routine ("bdd___bdd_subst_par" bdd_subst_par)
			 unsigned-int
  (f_vec (array unsigned-int))
  (vars unsigned-int)
  (g unsigned-int))

;;; BDDPTR bdd_subst_par_list (BDD_LIST f_list, BDD_LIST vars, BDDPTR g)
(alien:def-alien-routine ("bdd___bdd_subst_par_list" bdd_subst_par_list)
			 unsigned-int
  (f_list unsigned-int)
  (vars unsigned-int)
  (g unsigned-int))

;;; void bdd_free_vec (BDDPTR *f_vec, int size)
(alien:def-alien-routine ("bdd___bdd_free_vec" bdd_free_vec)
			 void
  (f_vec unsigned-int)
  (size integer))

;;; const char *bdd_get_output_string (int idx)
(alien:def-alien-routine ("bdd___bdd_get_output_string" bdd_get_output_string)
			 unsigned-int
  (idx integer))

;;; void bdd_set_output_string (int idx, const char *str)
(alien:def-alien-routine ("bdd___bdd_set_output_string" bdd_set_output_string)
			 void
  (idx integer)
  (str c-string))

;;; void bdd_print_as_sum_of_cubes (FILE *fp, BDDPTR f, int irredundant)
(alien:def-alien-routine ("bdd___bdd_print_as_sum_of_cubes" bdd_print_as_sum_of_cubes)
			 void
  (fp unsigned-int)
  (f unsigned-int)
  (irredundant unsigned-int))

;;; BDDPTR bdd_diff (BDDPTR f, BDD_LIST vars)
(alien:def-alien-routine ("bdd___bdd_diff" bdd_diff)
			 unsigned-int
  (f unsigned-int)
  (vars unsigned-int))

;;; BDDPTR bdd_one_of_vec (BDDPTR *vec, int size)
(alien:def-alien-routine ("bdd___bdd_one_of_vec" bdd_one_of_vec)
			 unsigned-int
  (vec unsigned-int)
  (size unsigned-int))

;;; BDDPTR bdd_none_of_vec (BDDPTR *args, int size)
(alien:def-alien-routine ("bdd___bdd_none_of_vec" bdd_none_of_vec)
			 unsigned-int
  (args unsigned-int)
  (size unsigned-int))

;;; BDDPTR bdd_subst (BDDPTR f, int var, BDDPTR g)
(alien:def-alien-routine ("bdd___bdd_subst" bdd_subst)
			 unsigned-int
  (f unsigned-int)
  (var unsigned-int)
  (g unsigned-int))

;;; BDD_LIST bdd_sum_of_cubes_as_list (BDDPTR f)
(alien:def-alien-routine ("bdd___bdd_sum_of_cubes_as_list" bdd_sum_of_cubes_as_list)
			 unsigned-int
  (f unsigned-int))

;;; int bdd_traverse_cube (BDDPTR cube,
;;;                        void (*action) (int index, int neg, int first))
(alien:def-alien-routine ("bdd___bdd_traverse_cube" bdd_traverse_cube)
			 unsigned-int
  (cube unsigned-int)
  (action unsigned-int))

;;; BDD_LIST bdd_support_as_list_of_vars (BDDPTR f)
(alien:def-alien-routine ("bdd___bdd_support_as_list_of_vars" bdd_support_as_list_of_vars)
			 unsigned-int
  (f unsigned-int))

(defun bdd-interrupted? ()
  ;; allegro breaks on signal 10 (bus error) unless we eval
  ;; Do the same here just to be on the safe side.
  (not (zerop (eval 'bdd_interrupted))))

(BDD_bdd_init)
