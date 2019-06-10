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

;;(shadow '(INT VOID UNSIGNED-INT))

;; (use-package :alien)
;;(use-package :c-call)

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
			 c-call:unsigned-int
  (x c-call:unsigned-int))
    
;;; void *elem_contents (LIST_ELEM_PTR x)
(alien:def-alien-routine ("bdd___elem_contents" elem_contents)
			 c-call:unsigned-int
  (x c-call:unsigned-int))
    
;;; LIST_ELEM_PTR list_first (LIST x)
(alien:def-alien-routine ("bdd___list_first" list_first)
			 c-call:unsigned-int
  (x c-call:unsigned-int))

;;; LIST_ELEM_PTR list_last (LIST x)
(alien:def-alien-routine ("bdd___list_last" list_last)
			 c-call:unsigned-int
  (x c-call:unsigned-int))

;;; LIST_INFO list_info (LIST x)
(alien:def-alien-routine ("bdd___list_info" list_info)
			 c-call:unsigned-int
  (x c-call:unsigned-int))
    
;;; LIST_ELEM_PTR list_next (LIST_ELEM_PTR x)
(alien:def-alien-routine ("bdd___list_next" list_next)
			 c-call:unsigned-int
  (x c-call:unsigned-int))

;;; This pretty much follows the bdd.doc sections.

;;; User settable program parameters
;;; --------------------------------
;;; int bdd_do_gc;	            /* default 1 */
;;; set_bdd_do_gc (int flag)
(alien:def-alien-routine ("bdd___set_bdd_do_gc" set_bdd_do_gc)
			 c-call:void
  (flag c-call:unsigned-int))

;;; int bdd_do_dynamic_ordering;/* default 1 */
;;; set_bdd_do_dynamic_ordering (int flag)
(alien:def-alien-routine ("bdd___set_bdd_do_dynamic_ordering" set_bdd_do_dynamic_ordering)
			 c-call:void
  (flag c-call:unsigned-int))
    
;;; int bdd_verbose;            /* default 0 */
;;; set_bdd_verbose (int flag)
(alien:def-alien-routine ("bdd___set_bdd_verbose" set_bdd_verbose)
			 c-call:void
  (flag c-call:unsigned-int))

;;; int bdd_use_neg_edges;      /* default 1*/
;;; set_bdd_use_neg_edges (int flag)
(alien:def-alien-routine ("bdd___set_bdd_use_neg_edges" set_bdd_use_neg_edges)
			 c-call:unsigned-int
  (flag c-call:unsigned-int))

;;; int bdd_use_inv_edges;  /* default 1; 0 when bdd_do_dynamic_ordering = 1 */
;;; set_bdd_use_inv_edges (int flag)
(alien:def-alien-routine ("bdd___set_bdd_use_inv_edges" set_bdd_use_inv_edges)
			 c-call:unsigned-int
  (flag c-call:unsigned-int))

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
			 c-call:unsigned-int
  (f c-call:unsigned-int))

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
			 c-call:unsigned-int
  (f c-call:unsigned-int))

;;; BDD_1_P (f)
;;; bdd_1_p (BDDPTR f)
(alien:def-alien-routine ("bdd___bdd_1_p" bdd_1_p)
			 c-call:unsigned-int
  (f c-call:unsigned-int))

;;; BDD_0_P (f)
;;; bdd_0_p (BDDPTR f)
(alien:def-alien-routine ("bdd___bdd_0_p" bdd_0_p)
			 c-call:unsigned-int
  (f c-call:unsigned-int))

;;; BDD_X_P (f)
;;; bdd_x_p (BDDPTR f)
(alien:def-alien-routine ("bdd___bdd_x_p" bdd_x_p)
			 c-call:unsigned-int
  (f c-call:unsigned-int))

;;; BDD_CONST_P (f)
;;; int bdd_const_p (BDDPTR f)
(alien:def-alien-routine ("bdd___bdd_const_p" bdd_const_p)
			 c-call:unsigned-int
  (f c-call:unsigned-int))

;;; BDD_TERM_P (f)
;;; bdd_term_p (BDDPTR f)
(alien:def-alien-routine ("bdd___bdd_term_p" bdd_term_p)
			 c-call:unsigned-int
  (f c-call:unsigned-int))

;;; BDD_LIT_P (f)
;;; bdd_lit_p (BDDPTR f)
(alien:def-alien-routine ("bdd___bdd_lit_p" bdd_lit_p)
			 c-call:unsigned-int
  (f c-call:unsigned-int))

;;; BDD_POSLIT_P (f)
;;; int bdd_poslit_p (BDDPTR f)
(alien:def-alien-routine ("bdd___bdd_poslit_p" bdd_poslit_p)
			 c-call:unsigned-int
  (f c-call:unsigned-int))

;;; BDD_NEGLIT_P (f)
;;; int bdd_neglit_p (BDDPTR f)
(alien:def-alien-routine ("bdd___bdd_neglit_p" bdd_neglit_p)
			 c-call:unsigned-int
  (f c-call:unsigned-int))

;;; BDD_COFACTOR_POS (f)
;;; BDDPTR bdd_cofactor_pos_ (BDDPTR f)
(alien:def-alien-routine ("bdd___bdd_cofactor_pos_" bdd_cofactor_pos_)
			 c-call:unsigned-int
  (f c-call:unsigned-int))

;;; BDD_COFACTOR_NEG (f)
;;; BDDPTR bdd_cofactor_neg_
(alien:def-alien-routine ("bdd___bdd_cofactor_neg_" bdd_cofactor_neg_)
			 c-call:unsigned-int
  (f c-call:unsigned-int))

;;; void bdd_reset_marks (BDDPTR f)
;;; void bdd_traverse_pre (register BDDPTR v, void (*pre_action)(BDDPTR))
;;; void bdd_traverse_post (register BDDPTR v, void (*post_action)(BDDPTR))

;;; int bdd_size (BDDPTR f)
(alien:def-alien-routine ("bdd___BDD_bdd_size" BDD_bdd_size)
			 c-call:unsigned-int
  (f c-call:unsigned-int))

;;; int bdd_size_vec (BDDPTR *f_vec, int size)
;;; int bdd_size_ceil (BDDPTR f, int ceiling)

;;; void bdd_init (void)
(alien:def-alien-routine ("bdd___BDD_bdd_init" BDD_bdd_init)
			 c-call:void)

;;; void bdd_free (BDDPTR f)
;; (alien:def-alien-routine ("bdd___bdd_free" bdd_free) void
;;   (f c-call:unsigned-int))

;; SO - for some reason, bdd_free causes problems.  Since we call bdd_init
;; each time anyway, this is only a problem for large BDDs, components of
;; which can't be freed during construction.
(defun bdd_free (ff) nil)

;;; int bdd_gc (void)
(alien:def-alien-routine ("bdd___bdd_gc" bdd_gc)
			 c-call:unsigned-int)

;;; BDDPTR bdd_ite (BDDPTR F, BDDPTR G, BDDPTR H)
(alien:def-alien-routine ("bdd___bdd_ite" bdd_ite)
			 c-call:unsigned-int
  (f c-call:unsigned-int)
  (g c-call:unsigned-int)
  (h c-call:unsigned-int))

;;; BDDPTR bdd_ite_const (BDDPTR F, BDDPTR G, BDDPTR H)
(alien:def-alien-routine ("bdd___bdd_ite_const" bdd_ite_const)
			 c-call:unsigned-int
  (f c-call:unsigned-int)
  (g c-call:unsigned-int)
  (h c-call:unsigned-int))

;;; void bdd_cofactors (BDDPTR f, BDDPTR *vp, BDDPTR *Tp, BDDPTR *Ep)
;;; BDDPTR bdd_invert_input_top (BDDPTR f)
(alien:def-alien-routine ("bdd___bdd_invert_input_top" bdd_invert_input_top)
			 c-call:unsigned-int
  (f c-call:unsigned-int))

;;; BDDPTR bdd_create_var (int v)
(alien:def-alien-routine ("bdd___bdd_create_var" bdd_create_var)
			 c-call:unsigned-int
  (v c-call:unsigned-int))

;;; BDDPTR bdd_create_var_first	(void)
(alien:def-alien-routine ("bdd___bdd_create_var_first" bdd_create_var_first)
			 c-call:unsigned-int
  )

;;; BDDPTR bdd_create_var_before (BDDPTR v)
(alien:def-alien-routine ("bdd___bdd_create_var_before" bdd_create_var_before)
			 c-call:unsigned-int
  (v c-call:unsigned-int))

;;; BDDPTR bdd_create_var_after	(BDDPTR v)
(alien:def-alien-routine ("bdd___bdd_create_var_after" bdd_create_var_after)
			 c-call:unsigned-int
  (v c-call:unsigned-int))

;;; BDDPTR bdd_create_var_last (void)
(alien:def-alien-routine ("bdd___bdd_create_var_last" bdd_create_var_last)
			 c-call:unsigned-int
  )

;;; void bdd_print (FILE *fp, BDDPTR f, char *s)
(alien:def-alien-routine ("bdd___bdd_print" bdd_print)
			 c-call:void
  (fp c-call:unsigned-int)
  (f c-call:unsigned-int)
  (s c-call:c-string))

;;; void bdd_print_stats (FILE *fp)
;;; void bdd_quit (void)
(alien:def-alien-routine ("bdd___bdd_quit" bdd_quit)
			 c-call:void
  )

;;; int bdd_memsize (void)
;;; int bdd_memsize_limit (void)
;;; void bdd_set_memsize_limit_and_handler (int limit, void (*handler) (void))
;;; int bdd_nodes_alive (void)
(alien:def-alien-routine ("bdd___bdd_nodes_alive" bdd_nodes_alive)
			 c-call:unsigned-int
  )

;;; int bdd_nodes_allocated (void)
(alien:def-alien-routine ("bdd___bdd_nodes_allocated" bdd_nodes_allocated)
			 c-call:unsigned-int
  )

;;; int bdd_nr_occurs_var (int id)
;;; int bdd_compl_p (BDDPTR f, BDDPTR g)
;;; int bdd_equal_p (BDDPTR F, BDDPTR G)
(alien:def-alien-routine ("bdd___bdd_equal_p" bdd_equal_p)
			 c-call:unsigned-int
  (f c-call:unsigned-int)
  (g c-call:unsigned-int))

;;; int bdd_implies_taut (BDDPTR F, BDDPTR G)
;;; BDDPTR bdd_not (BDDPTR F)
(alien:def-alien-routine ("bdd___bdd_not" bdd_not)
			 c-call:unsigned-int
  (f c-call:unsigned-int))

;;; BDDPTR bdd_and (BDDPTR F, BDDPTR G)
(alien:def-alien-routine ("bdd___bdd_and" bdd_and)
			 c-call:unsigned-int
  (f c-call:unsigned-int)
  (g c-call:unsigned-int))

;;; BDDPTR bdd_greater	(BDDPTR F, BDDPTR G)
(alien:def-alien-routine ("bdd___bdd_greater" bdd_greater)
			 c-call:unsigned-int
  (f c-call:unsigned-int)
  (g c-call:unsigned-int))

;;; BDDPTR bdd_less (BDDPTR F, BDDPTR G)
(alien:def-alien-routine ("bdd___bdd_less" bdd_less)
			 c-call:unsigned-int
  (f c-call:unsigned-int)
  (g c-call:unsigned-int))

;;; BDDPTR bdd_xor (BDDPTR F, BDDPTR G)
(alien:def-alien-routine ("bdd___bdd_xor" bdd_xor)
			 c-call:unsigned-int
  (f c-call:unsigned-int)
  (g c-call:unsigned-int))

;;; BDDPTR bdd_or (BDDPTR F, BDDPTR G)
(alien:def-alien-routine ("bdd___bdd_or" bdd_or)
			 c-call:unsigned-int
  (f c-call:unsigned-int)
  (g c-call:unsigned-int))

;;; BDDPTR bdd_nor (BDDPTR F, BDDPTR G)
(alien:def-alien-routine ("bdd___bdd_nor" bdd_nor)
			 c-call:unsigned-int
  (f c-call:unsigned-int)
  (g c-call:unsigned-int))

;;; BDDPTR bdd_equiv (BDDPTR F, BDDPTR G)
(alien:def-alien-routine ("bdd___bdd_equiv" bdd_equiv)
			 c-call:unsigned-int
  (f c-call:unsigned-int)
  (g c-call:unsigned-int))

;;; BDDPTR bdd_xnor (BDDPTR F, BDDPTR G) /* equivalent to bdd_equiv */
(alien:def-alien-routine ("bdd___bdd_xnor" bdd_xnor)
			 c-call:unsigned-int
  (f c-call:unsigned-int)
  (g c-call:unsigned-int))

;;; BDDPTR bdd_implied (BDDPTR F, BDDPTR G)
(alien:def-alien-routine ("bdd___bdd_implied" bdd_implied)
			 c-call:unsigned-int
  (f c-call:unsigned-int)
  (g c-call:unsigned-int))

;;; BDDPTR bdd_implies (BDDPTR F, BDDPTR G)
(alien:def-alien-routine ("bdd___bdd_implies" bdd_implies)
			 c-call:unsigned-int
  (f c-call:unsigned-int)
  (g c-call:unsigned-int))

;;; BDDPTR bdd_nand (BDDPTR F, BDDPTR G)
(alien:def-alien-routine ("bdd___bdd_nand" bdd_nand)
			 c-call:unsigned-int
  (f c-call:unsigned-int)
  (g c-call:unsigned-int))

;;; BDDPTR bdd_0 (void)
(alien:def-alien-routine ("bdd___bdd_0" bdd_0)
			 c-call:unsigned-int
  )

;;; BDDPTR bdd_1 (void)
(alien:def-alien-routine ("bdd___bdd_1" bdd_1)
			 c-call:unsigned-int
  )

;;; BDDPTR bdd_X (void)
(alien:def-alien-routine ("bdd___bdd_X" bdd_X)
			 c-call:unsigned-int)

;;; BDDPTR bdd_assign (BDDPTR f)
(alien:def-alien-routine ("bdd___bdd_assign" bdd_assign)
			 c-call:unsigned-int
  (f c-call:unsigned-int))

;;; BDDPTR bdd_top_var (BDDPTR f)
;;; int bdd_top_var_rank (BDDPTR f)
;;; BDDPTR bdd_then (BDDPTR f)
(alien:def-alien-routine ("bdd___BDD_bdd_then" BDD_bdd_then)
			 c-call:unsigned-int
  (f c-call:unsigned-int))

;;; BDDPTR bdd_else (BDDPTR f)
(alien:def-alien-routine ("bdd___BDD_bdd_else" BDD_bdd_else)
			 c-call:unsigned-int
  (f c-call:unsigned-int))

;;; BDDPTR bdd_apply (BDDPTR (*f)(BDDPTR,BDDPTR),BDDPTR a,BDDPTR b)
(alien:def-alien-routine ("bdd___bdd_apply" bdd_apply)
			 c-call:unsigned-int
  (f c-call:unsigned-int)
  (a c-call:unsigned-int)
  (b c-call:unsigned-int))

;;; BDDPTR bdd_constrain (BDDPTR f, BDDPTR c)
(alien:def-alien-routine ("bdd___bdd_constrain" bdd_constrain)
			 c-call:unsigned-int
  (f c-call:unsigned-int)
  (c c-call:unsigned-int))

;;; BDDPTR bdd_top_var (BDDPTR f)
(alien:def-alien-routine ("bdd___bdd_top_var" bdd_top_var)
			 c-call:unsigned-int
  (f c-call:unsigned-int))

;;; BDD_LIST bdd_sum_of_cubes (BDDPTR f, int irredundant)
(alien:def-alien-routine ("bdd___bdd_sum_of_cubes" bdd_sum_of_cubes)
			 c-call:unsigned-int
  (f c-call:unsigned-int)
  (irredundant c-call:unsigned-int))

(alien:def-alien-variable bdd_interrupted integer)

;;; The following were obtained by looking through mu.c and collecting
;;; functions not mentioned above.

;;; int bdd_reorder_var (int var_id, int target_var_id)
(alien:def-alien-routine ("bdd___bdd_reorder_var" bdd_reorder_var)
			 c-call:unsigned-int
  (var_id c-call:unsigned-int)
  (target_var c-call:unsigned-int))

;;; BDDPTR bdd_and_smooth (BDDPTR f, BDDPTR g, BDD_LIST vars)
(alien:def-alien-routine ("bdd___bdd_and_smooth" bdd_and_smooth)
			 c-call:unsigned-int
  (f c-call:unsigned-int)
  (g c-call:unsigned-int)
  (vars c-call:unsigned-int))

;;; BDD_LIST bdd_rank_order_vars (BDD_LIST vars)
(alien:def-alien-routine ("bdd___bdd_rank_order_vars" bdd_rank_order_vars)
			 c-call:unsigned-int
  (vars c-call:unsigned-int))

;;; BDDPTR bdd_quantify (int existential, BDDPTR f, BDD_LIST vars)
(alien:def-alien-routine ("bdd___bdd_quantify" bdd_quantify)
			 c-call:unsigned-int
  (existential c-call:unsigned-int)
  (f c-call:unsigned-int)
  (vars c-call:unsigned-int))

;;; BDDPTR bdd_subst_par (BDDPTR *f_vec, BDD_LIST vars, BDDPTR g)
(alien:def-alien-routine ("bdd___bdd_subst_par" bdd_subst_par)
			 c-call:unsigned-int
  (f_vec (array c-call:unsigned-int))
  (vars c-call:unsigned-int)
  (g c-call:unsigned-int))

;;; BDDPTR bdd_subst_par_list (BDD_LIST f_list, BDD_LIST vars, BDDPTR g)
(alien:def-alien-routine ("bdd___bdd_subst_par_list" bdd_subst_par_list)
			 c-call:unsigned-int
  (f_list c-call:unsigned-int)
  (vars c-call:unsigned-int)
  (g c-call:unsigned-int))

;;; void bdd_free_vec (BDDPTR *f_vec, int size)
(alien:def-alien-routine ("bdd___bdd_free_vec" bdd_free_vec)
			 c-call:void
  (f_vec c-call:unsigned-int)
  (size integer))

;;; const char *bdd_get_output_string (int idx)
(alien:def-alien-routine ("bdd___bdd_get_output_string" bdd_get_output_string)
			 c-call:unsigned-int
  (idx integer))

;;; void bdd_set_output_string (int idx, const char *str)
(alien:def-alien-routine ("bdd___bdd_set_output_string" bdd_set_output_string)
			 c-call:void
  (idx integer)
  (str c-call:c-string))

;;; void bdd_print_as_sum_of_cubes (FILE *fp, BDDPTR f, int irredundant)
(alien:def-alien-routine ("bdd___bdd_print_as_sum_of_cubes" bdd_print_as_sum_of_cubes)
			 c-call:void
  (fp c-call:unsigned-int)
  (f c-call:unsigned-int)
  (irredundant c-call:unsigned-int))

;;; BDDPTR bdd_diff (BDDPTR f, BDD_LIST vars)
(alien:def-alien-routine ("bdd___bdd_diff" bdd_diff)
			 c-call:unsigned-int
  (f c-call:unsigned-int)
  (vars c-call:unsigned-int))

;;; BDDPTR bdd_one_of_vec (BDDPTR *vec, int size)
(alien:def-alien-routine ("bdd___bdd_one_of_vec" bdd_one_of_vec)
			 c-call:unsigned-int
  (vec c-call:unsigned-int)
  (size c-call:unsigned-int))

;;; BDDPTR bdd_none_of_vec (BDDPTR *args, int size)
(alien:def-alien-routine ("bdd___bdd_none_of_vec" bdd_none_of_vec)
			 c-call:unsigned-int
  (args c-call:unsigned-int)
  (size c-call:unsigned-int))

;;; BDDPTR bdd_subst (BDDPTR f, int var, BDDPTR g)
(alien:def-alien-routine ("bdd___bdd_subst" bdd_subst)
			 c-call:unsigned-int
  (f c-call:unsigned-int)
  (var c-call:unsigned-int)
  (g c-call:unsigned-int))

;;; BDD_LIST bdd_sum_of_cubes_as_list (BDDPTR f)
(alien:def-alien-routine ("bdd___bdd_sum_of_cubes_as_list" bdd_sum_of_cubes_as_list)
			 c-call:unsigned-int
  (f c-call:unsigned-int))

;;; int bdd_traverse_cube (BDDPTR cube,
;;;                        void (*action) (int index, int neg, int first))
(alien:def-alien-routine ("bdd___bdd_traverse_cube" bdd_traverse_cube)
			 c-call:unsigned-int
  (cube c-call:unsigned-int)
  (action c-call:unsigned-int))

;;; BDD_LIST bdd_support_as_list_of_vars (BDDPTR f)
(alien:def-alien-routine ("bdd___bdd_support_as_list_of_vars" bdd_support_as_list_of_vars)
			 c-call:unsigned-int
  (f c-call:unsigned-int))

(defun bdd-interrupted? ()
  ;; allegro breaks on signal 10 (bus error) unless we eval
  ;; Do the same here just to be on the safe side.
  (not (zerop (eval 'bdd_interrupted))))

(BDD_bdd_init)
