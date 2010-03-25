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
(sb-alien:define-alien-routine ("bdd___null_list_p" null_list_p)
			       (integer 32)
  (x (* t)))

;;; void *elem_contents (LIST_ELEM_PTR x)
(sb-alien:define-alien-routine ("bdd___elem_contents" elem_contents)
			       (* t)
  (x (* t)))

;;; LIST_ELEM_PTR list_first (LIST x)
(sb-alien:define-alien-routine ("bdd___list_first" list_first)
			       (* t)
  (x (* t)))

;;; LIST_ELEM_PTR list_last (LIST x)
(sb-alien:define-alien-routine ("bdd___list_last" list_last)
			       (* t)
  (x (* t)))

;;; int list_info (LIST x)
(sb-alien:define-alien-routine ("bdd___list_info" list_info)
			       (integer 32)
  (x (* t)))

;;; LIST_ELEM_PTR list_next (LIST_ELEM_PTR x)
(sb-alien:define-alien-routine ("bdd___list_next" list_next)
			       (* t)
  (x (* t)))

;;; This pretty much follows the bdd.doc sections.

;;; User settable program parameters
;;; --------------------------------
;;; int bdd_do_gc;	            /* default 1 */

(sb-alien:define-alien-variable "bdd_do_gc" (integer 32))

;;; set_bdd_do_gc (int flag)
(declaim (inline set_bdd_do_gc))
(defun set_bdd_do_gc (flag)
  (setf bdd-do-gc flag))

;;; int bdd_do_dynamic_ordering;/* default 1 */
(sb-alien:define-alien-variable "bdd_do_dynamic_ordering" (integer 32))

;;; set_bdd_do_dynamic_ordering (int flag)
(declaim (inline set_bdd_do_dynamic_ordering))
(defun set_bdd_do_dynamic_ordering (flag)
  (setf bdd-do-dynamic-ordering flag))

;;; int bdd_verbose;            /* default 0 */
(sb-alien:define-alien-variable "bdd_verbose" (integer 32))

;;; set_bdd_verbose (int flag)
(declaim (inline set_bdd_verbose))
(defun set_bdd_verbose (flag)
  (setf bdd-verbose flag))

;;; int bdd_use_neg_edges;      /* default 1*/
(sb-alien:define-alien-variable "bdd_use_neg_edges" (integer 32))

;;; set_bdd_use_neg_edges (int flag)
(declaim (inline set_bdd_use_neg_edges))
(defun set_bdd_use_neg_edges (flag)
  (setf bdd-use-neg-edges flag))

;;; int bdd_use_inv_edges;  /* default 1; 0 when bdd_do_dynamic_ordering = 1 */
(sb-alien:define-alien-variable "bdd_use_inv_edges" (integer 32))

;;; set_bdd_use_inv_edges (int flag)
(declaim (inline set_bdd_use_inv_edges))
(defun set_bdd_use_inv_edges (flag)
  (setf bdd-use-inv-edges flag))

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
(sb-alien:define-alien-routine ("bdd___bdd_varid" bdd_varid)
			       (sb-alien:unsigned 32)
  (f (* t)))

;;; BDD_THEN (F)
;;; BDD_ELSE (F)
;;; BDD_REFCOUNT (F)
;;; BDD_FLAG (F)
;;; BDD_MARK (F)


;;; Test on terminal nodes:
;;; -----------------------

;;; BDD_VOID_P (f)
;;; int bdd_void_p (BDDPTR f)
(sb-alien:define-alien-routine ("bdd___bdd_void_p" bdd_void_p)
			       (integer 32)
  (f (* t)))

;;; BDD_1_P (f)
;;; bdd_1_p (BDDPTR f)
(sb-alien:define-alien-routine ("bdd___bdd_1_p" bdd_1_p)
			       (integer 32)
  (f (* t)))

;;; BDD_0_P (f)
;;; bdd_0_p (BDDPTR f)
(sb-alien:define-alien-routine ("bdd___bdd_0_p" bdd_0_p)
			       (integer 32)
  (f (* t)))

;;; BDD_X_P (f)
;;; bdd_x_p (BDDPTR f)
(sb-alien:define-alien-routine ("bdd___bdd_x_p" bdd_x_p)
			       (integer 32)
  (f (* t)))

;;; BDD_CONST_P (f)
;;; int bdd_const_p (BDDPTR f)
(sb-alien:define-alien-routine ("bdd___bdd_const_p" bdd_const_p)
			       (integer 32)
  (f (* t)))

;;; BDD_TERM_P (f)
;;; bdd_term_p (BDDPTR f)
(sb-alien:define-alien-routine ("bdd___bdd_term_p" bdd_term_p)
			       (integer 32)
  (f (* t)))

;;; BDD_LIT_P (f)
;;; bdd_lit_p (BDDPTR f)
(sb-alien:define-alien-routine ("bdd___bdd_lit_p" bdd_lit_p)
			       (integer 32)
  (f (* t)))

;;; BDD_POSLIT_P (f)
;;; int bdd_poslit_p (BDDPTR f)
(sb-alien:define-alien-routine ("bdd___bdd_poslit_p" bdd_poslit_p)
			       (integer 32)
  (f (* t)))

;;; BDD_NEGLIT_P (f)
;;; int bdd_neglit_p (BDDPTR f)
(sb-alien:define-alien-routine ("bdd___bdd_neglit_p" bdd_neglit_p)
			       (integer 32)
  (f (* t)))

;;; BDD_COFACTOR_POS (f)
;;; BDDPTR bdd_cofactor_pos_ (BDDPTR f)
(sb-alien:define-alien-routine ("bdd___bdd_cofactor_pos_" bdd_cofactor_pos_)
			       (* t)
  (f (* t)))

;;; BDD_COFACTOR_NEG (f)
;;; BDDPTR bdd_cofactor_neg_ (BDDPTR f)
(sb-alien:define-alien-routine ("bdd___bdd_cofactor_neg_" bdd_cofactor_neg_)
			       (* t)
  (f (* t)))

;;; void bdd_reset_marks (BDDPTR f)
;;; void bdd_traverse_pre (register BDDPTR v, void (*pre_action)(BDDPTR))
;;; void bdd_traverse_post (register BDDPTR v, void (*post_action)(BDDPTR))

;;; int BDD_bdd_size (BDDPTR f)
(sb-alien:define-alien-routine ("bdd___BDD_bdd_size" BDD_bdd_size)
			       (integer 32)
  (f (* t)))

;;; int bdd_size_vec (BDDPTR *f_vec, int size)
;;; int bdd_size_ceil (BDDPTR f, int ceiling)

;;; void BDD_bdd_init (void)
(sb-alien:define-alien-routine ("bdd___BDD_bdd_init" BDD_bdd_init)
			       sb-alien:void)

;;; void bdd_free (BDDPTR f)
(sb-alien:define-alien-routine ("bdd___bdd_free" bdd_free)
			       sb-alien:void
  (f (* t)))

;;; int bdd_gc (void)
(sb-alien:define-alien-routine ("bdd___bdd_gc" bdd_gc)
			       (integer 32))

;;; BDDPTR bdd_ite (BDDPTR F, BDDPTR G, BDDPTR H)
(sb-alien:define-alien-routine ("bdd___bdd_ite" bdd_ite)
			       (* t)
  (f (* t))
  (g (* t))
  (h (* t)))

;;; BDDPTR bdd_ite_const (BDDPTR F, BDDPTR G, BDDPTR H)
(sb-alien:define-alien-routine ("bdd___bdd_ite_const" bdd_ite_const)
			       (* t)
  (f (* t))
  (g (* t))
  (h (* t)))

;;; void bdd_cofactors (BDDPTR f, BDDPTR *vp, BDDPTR *Tp, BDDPTR *Ep)
;;; BDDPTR bdd_invert_input_top (BDDPTR f)
(sb-alien:define-alien-routine
 ("bdd___bdd_invert_input_top" bdd_invert_input_top)
			       (* t)
  (f (* t)))

;;; BDDPTR bdd_create_var (int v)
(sb-alien:define-alien-routine ("bdd___bdd_create_var" bdd_create_var)
			       (* t)
  (v (integer 32)))

;;; BDDPTR bdd_create_var_first	(void)
(sb-alien:define-alien-routine
 ("bdd___bdd_create_var_first" bdd_create_var_first)
			       (* t))

;;; BDDPTR bdd_create_var_before (BDDPTR v)
(sb-alien:define-alien-routine
 ("bdd___bdd_create_var_before" bdd_create_var_before)
			       (* t)
  (v (* t)))

;;; BDDPTR bdd_create_var_after	(BDDPTR v)
(sb-alien:define-alien-routine
 ("bdd___bdd_create_var_after" bdd_create_var_after)
			       (* t)
  (v (* t)))

;;; BDDPTR bdd_create_var_last (void)
(sb-alien:define-alien-routine
 ("bdd___bdd_create_var_last" bdd_create_var_last)
			       (* t))

;;; void bdd_print (FILE *fp, BDDPTR f, char *s)
(sb-alien:define-alien-routine ("bdd___bdd_print" bdd_print)
			       sb-alien:void
  (fp (* t))
  (f (* t))
  (s sb-alien:c-string))

;;; void bdd_print_stats (FILE *fp)
;;; void bdd_quit (void)
(sb-alien:define-alien-routine ("bdd___bdd_quit" bdd_quit)
			       sb-alien:void)

;;; int bdd_memsize (void)
;;; int bdd_memsize_limit (void)
;;; void bdd_set_memsize_limit_and_handler (int limit, void (*handler) (void))
;;; int bdd_nodes_alive (void)
(sb-alien:define-alien-routine ("bdd___bdd_nodes_alive" bdd_nodes_alive)
			       (integer 32))

;;; int bdd_nodes_allocated (void)
(sb-alien:define-alien-routine
 ("bdd___bdd_nodes_allocated" bdd_nodes_allocated)
			       (integer 32))

;;; int bdd_nr_occurs_var (int id)
;;; int bdd_compl_p (BDDPTR f, BDDPTR g)
;;; int bdd_equal_p (BDDPTR F, BDDPTR G)
(sb-alien:define-alien-routine ("bdd___bdd_equal_p" bdd_equal_p)
			       (integer 32)
  (f (* t))
  (g (* t)))

;;; int bdd_implies_taut (BDDPTR F, BDDPTR G)
;;; BDDPTR bdd_not (BDDPTR F)
(sb-alien:define-alien-routine ("bdd___bdd_not" bdd_not)
			       (* t)
  (f (* t)))

;;; BDDPTR bdd_and (BDDPTR F, BDDPTR G)
(sb-alien:define-alien-routine ("bdd___bdd_and" bdd_and)
			       (* t)
  (f (* t))
  (g (* t)))

;;; BDDPTR bdd_greater	(BDDPTR F, BDDPTR G)
(sb-alien:define-alien-routine ("bdd___bdd_greater" bdd_greater)
			       (* t)
  (f (* t))
  (g (* t)))

;;; BDDPTR bdd_less (BDDPTR F, BDDPTR G)
(sb-alien:define-alien-routine ("bdd___bdd_less" bdd_less)
			       (* t)
  (f (* t))
  (g (* t)))

;;; BDDPTR bdd_xor (BDDPTR F, BDDPTR G)
(sb-alien:define-alien-routine ("bdd___bdd_xor" bdd_xor)
			       (* t)
  (f (* t))
  (g (* t)))

;;; BDDPTR bdd_or (BDDPTR F, BDDPTR G)
(sb-alien:define-alien-routine ("bdd___bdd_or" bdd_or)
			       (* t)
  (f (* t))
  (g (* t)))

;;; BDDPTR bdd_nor (BDDPTR F, BDDPTR G)
(sb-alien:define-alien-routine ("bdd___bdd_nor" bdd_nor)
			       (* t)
  (f (* t))
  (g (* t)))

;;; BDDPTR bdd_equiv (BDDPTR F, BDDPTR G)
(sb-alien:define-alien-routine ("bdd___bdd_equiv" bdd_equiv)
			       (* t)
  (f (* t))
  (g (* t)))

;;; BDDPTR bdd_xnor (BDDPTR F, BDDPTR G) /* equivalent to bdd_equiv */
(sb-alien:define-alien-routine ("bdd___bdd_xnor" bdd_xnor)
			       (* t)
  (f (* t))
  (g (* t)))

;;; BDDPTR bdd_implied (BDDPTR F, BDDPTR G)
(sb-alien:define-alien-routine ("bdd___bdd_implied" bdd_implied)
			       (* t)
  (f (* t))
  (g (* t)))

;;; BDDPTR bdd_implies (BDDPTR F, BDDPTR G)
(sb-alien:define-alien-routine ("bdd___bdd_implies" bdd_implies)
			       (* t)
  (f (* t))
  (g (* t)))

;;; BDDPTR bdd_nand (BDDPTR F, BDDPTR G)
(sb-alien:define-alien-routine ("bdd___bdd_nand" bdd_nand)
			       (* t)
  (f (* t))
  (g (* t)))

;;; BDDPTR bdd_0 (void)
(sb-alien:define-alien-routine ("bdd___bdd_0" bdd_0)
			       (* t))

;;; BDDPTR bdd_1 (void)
(sb-alien:define-alien-routine ("bdd___bdd_1" bdd_1)
			       (* t))

;;; BDDPTR bdd_X (void)
(sb-alien:define-alien-routine ("bdd___bdd_X" bdd_X)
			       (* t))

;;; BDDPTR bdd_assign (BDDPTR f)
(sb-alien:define-alien-routine ("bdd___bdd_assign" bdd_assign)
			       (* t)
  (f (* t)))

;;; BDDPTR bdd_top_var (BDDPTR f)
;;; int bdd_top_var_rank (BDDPTR f)
;;; BDDPTR bdd_then (BDDPTR f)
(sb-alien:define-alien-routine ("bdd___BDD_bdd_then" BDD_bdd_then)
			       (* t)
  (f (* t)))

;;; BDDPTR bdd_else (BDDPTR f)
(sb-alien:define-alien-routine ("bdd___BDD_bdd_else" BDD_bdd_else)
			       (* t)
  (f (* t)))

;;; BDDPTR bdd_apply (BDDPTR (*f)(BDDPTR,BDDPTR),BDDPTR a,BDDPTR b)
(sb-alien:define-alien-routine ("bdd___bdd_apply" bdd_apply)
			       (* t)
  (f (* t))
  (a (* t))
  (b (* t)))

;;; BDDPTR bdd_constrain (BDDPTR f, BDDPTR c)
(sb-alien:define-alien-routine ("bdd___bdd_constrain" bdd_constrain)
			       (* t)
  (f (* t))
  (c (* t)))

;;; BDDPTR bdd_top_var (BDDPTR f)
(sb-alien:define-alien-routine ("bdd___bdd_top_var" bdd_top_var)
			       (* t)
  (f (* t)))

;;; BDD_LIST bdd_sum_of_cubes (BDDPTR f, int irredundant)
(sb-alien:define-alien-routine ("bdd___bdd_sum_of_cubes" bdd_sum_of_cubes)
			       (* t)
  (f (* t))
  (irredundant (integer 32)))

(sb-alien:define-alien-variable ("bdd_interrupted" bdd_interrupted) (integer 32))

;;; The following were obtained by looking through mu.c and collecting
;;; functions not mentioned above.

;;; int bdd_reorder_var (int var_id, int target_var_id)
(sb-alien:define-alien-routine ("bdd___bdd_reorder_var" bdd_reorder_var)
			       (integer 32)
  (var_id (integer 32))
  (target_var (integer 32)))

;;; BDDPTR bdd_and_smooth (BDDPTR f, BDDPTR g, BDD_LIST vars)
(sb-alien:define-alien-routine ("bdd___bdd_and_smooth" bdd_and_smooth)
			       (* t)
  (f (* t))
  (g (* t))
  (vars (* t)))

;;; BDD_LIST bdd_rank_order_vars (BDD_LIST vars)
(sb-alien:define-alien-routine
 ("bdd___bdd_rank_order_vars" bdd_rank_order_vars)
			       (* t)
  (vars (* t)))

;;; BDDPTR bdd_quantify (int existential, BDDPTR f, BDD_LIST vars)
(sb-alien:define-alien-routine ("bdd___bdd_quantify" bdd_quantify)
			       (* t)
  (existential (integer 32))
  (f (* t))
  (vars (* t)))

;;; BDDPTR bdd_subst_par (BDDPTR *f_vec, BDD_LIST vars, BDDPTR g)
(sb-alien:define-alien-routine ("bdd___bdd_subst_par" bdd_subst_par)
			       (* t)
  (f_vec (array (* t)))
  (vars (* t))
  (g (* t)))

;;; BDDPTR bdd_subst_par_list (BDD_LIST f_list, BDD_LIST vars, BDDPTR g)
(sb-alien:define-alien-routine ("bdd___bdd_subst_par_list" bdd_subst_par_list)
			       (* t)
  (f_list (* t))
  (vars (* t))
  (g (* t)))

;;; void bdd_free_vec (BDDPTR *f_vec, int size)
(sb-alien:define-alien-routine ("bdd___bdd_free_vec" bdd_free_vec)
			       sb-alien:void
  (f_vec (array (* t)))
  (size (integer 32)))

;;; const char *bdd_get_output_string (int idx)
(sb-alien:define-alien-routine
 ("bdd___bdd_get_output_string" bdd_get_output_string)
			       sb-alien:c-string
  (idx (integer 32)))

;;; void bdd_set_output_string (int idx, const char *str)
(sb-alien:define-alien-routine
 ("bdd___bdd_set_output_string" bdd_set_output_string)
			       sb-alien:void
  (idx (integer 32))
  (str sb-alien:c-string))

;;; void bdd_print_as_sum_of_cubes (FILE *fp, BDDPTR f, int irredundant)
(sb-alien:define-alien-routine
 ("bdd___bdd_print_as_sum_of_cubes" bdd_print_as_sum_of_cubes)
			       sb-alien:void
  (fp (* t))
  (f (* t))
  (irredundant (integer 32)))

;;; BDDPTR bdd_diff (BDDPTR f, BDD_LIST vars)
(sb-alien:define-alien-routine ("bdd___bdd_diff" bdd_diff)
			       (* t)
  (f (* t))
  (vars (* t)))

;;; BDDPTR bdd_one_of_vec (BDDPTR *vec, int size)
(sb-alien:define-alien-routine ("bdd___bdd_one_of_vec" bdd_one_of_vec)
			       (* t)
  (vec (array (* t)))
  (size (integer 32)))

;;; BDDPTR bdd_none_of_vec (BDDPTR *args, int size)
(sb-alien:define-alien-routine ("bdd___bdd_none_of_vec" bdd_none_of_vec)
			       (* t)
  (args (array (* t)))
  (size (integer 32)))

;;; BDDPTR bdd_subst (BDDPTR f, int var, BDDPTR g)
(sb-alien:define-alien-routine ("bdd___bdd_subst" bdd_subst)
			       (* t)
  (f (* t))
  (var (integer 32))
  (g (* t)))

;;; BDD_LIST bdd_sum_of_cubes_as_list (BDDPTR f)
(sb-alien:define-alien-routine
 ("bdd___bdd_sum_of_cubes_as_list" bdd_sum_of_cubes_as_list)
			       (* t)
  (f (* t)))

;;; int bdd_traverse_cube (BDDPTR cube,
;;;                        void (*action) (int index, int neg, int first))
(sb-alien:define-alien-routine ("bdd___bdd_traverse_cube" bdd_traverse_cube)
			       (integer 32)
  (cube (* t))
  (action (* t)))

;;; BDD_LIST bdd_support_as_list_of_vars (BDDPTR f)
(sb-alien:define-alien-routine
 ("bdd___bdd_support_as_list_of_vars" bdd_support_as_list_of_vars)
			       (* t)
  (f (* t)))

(defun bdd-interrupted? ()
  (not (zerop bdd_interrupted)))

(BDD_bdd_init)
