;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bdd-cffi.lisp -- Interface to the BDD package
;;   Provides the foreign functions for Allegro CL
;; Author          : Sree, Shankar and Saidi
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(cffi:define-foreign-library pvs
    (t (:default "mu")))

(cffi:defcvar bdd_interrupted :int)

;;; List accessors
;;; Lists in the BDD package involve two structures.

;;; A LIST is a structure with slots for a first element pointer, a last
;;; element pointer, the size, and user-defined info.

;;; A LIST_ELEM is a structure with slots for the contents and the next
;;; element.

;;; #define NULL_LIST ((LIST) 0)

;;; int null_list_p (LIST x)
(cffi:defcfun (null_list_p "bdd___null_list_p") :int32
  (x :pointer))

;;; void *elem_contents (LIST_ELEM_PTR x)
(cffi:defcfun (elem_contents "bdd___elem_contents") :pointer
  (x :pointer))
  
;;; LIST_ELEM_PTR list_first (LIST x)
(cffi:defcfun (list_first "bdd___list_first") :pointer
  (x :pointer))

;;; LIST_ELEM_PTR list_last (LIST x)
(cffi:defcfun (list_last "bdd___list_last") :pointer
  (x :pointer))
  
;;; LIST_INFO list_info (LIST x)
(cffi:defcfun (list_info "bdd___list_info") :int32
  (x :pointer))

;;; LIST_ELEM_PTR list_next (LIST_ELEM_PTR x)
(cffi:defcfun (list_next "bdd___list_next") :pointer
  (x :pointer))

;;; This pretty much follows the bdd.doc sections.

;;; User settable program parameters
;;; --------------------------------
;;; int bdd_do_gc;	            /* default 1 */
;;; set_bdd_do_gc (int flag)
(cffi:defcfun (set_bdd_do_gc "bdd___set_bdd_do_gc") :void
  (flag :int32))

;;; int bdd_do_dynamic_ordering;/* default 1 */
;;; set_bdd_do_dynamic_ordering (int flag)
(cffi:defcfun (set_bdd_do_dynamic_ordering "bdd___set_bdd_do_dynamic_ordering")
    :int32
  (flag :int32))

;;; int bdd_verbose;            /* default 0 */
;;; set_bdd_verbose (int flag)
(cffi:defcfun (set_bdd_verbose "bdd___set_bdd_verbose") :void
  (flag :int32))

;;; int bdd_use_neg_edges;      /* default 1*/
;;; set_bdd_use_neg_edges (int flag)
(cffi:defcfun (set_bdd_use_neg_edges "bdd___set_bdd_use_neg_edges") :int32
  (flag :int32))

;;; int bdd_use_inv_edges;  /* default 1; 0 when bdd_do_dynamic_ordering = 1 */
;;; set_bdd_use_inv_edges (int flag)
(cffi:defcfun (set_bdd_use_inv_edges "bdd___set_bdd_use_inv_edges") :int32
  (flag :int32))

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
(cffi:defcfun (bdd_varid "bdd___bdd_varid") :uint32
  (f :pointer))

;;; BDD_THEN (F)
;;; BDD_ELSE (F)
;;; BDD_REFCOUNT (F)
;;; BDD_FLAG (F)
;;; BDD_MARK (F)


;;; Test on terminal nodes:
;;; -----------------------

;;; BDD_VOID_P (f)
;;; int bdd_void_p (BDDPTR f)
(cffi:defcfun (bdd_void_p "bdd___bdd_void_p") :int32
    (f :pointer))

;;; BDD_1_P (f)
;;; bdd_1_p (BDDPTR f)
(cffi:defcfun (bdd_1_p "bdd___bdd_1_p") :int32
    (f :pointer))

;;; BDD_0_P (f)
;;; bdd_0_p (BDDPTR f)
(cffi:defcfun (bdd_0_p "bdd___bdd_0_p") :int32
  (f :pointer))

;;; BDD_X_P (f)
;;; bdd_x_p (BDDPTR f)
(cffi:defcfun (bdd_x_p "bdd___bdd_x_p") :int32
  (f :pointer))

;;; BDD_CONST_P (f)
;;; int bdd_const_p (BDDPTR f)
(cffi:defcfun (bdd_const_p "bdd___bdd_const_p") :int32
  (f :pointer))

;;; BDD_TERM_P (f)
;;; bdd_term_p (BDDPTR f)
(cffi:defcfun (bdd_term_p "bdd___bdd_term_p") :int32
  (f :pointer))

;;; BDD_LIT_P (f)
;;; bdd_lit_p (BDDPTR f)
(cffi:defcfun (bdd_lit_p "bdd___bdd_lit_p") :int32
  (f :pointer))

;;; BDD_POSLIT_P (f)
;;; int bdd_poslit_p (BDDPTR f)
(cffi:defcfun (bdd_poslit_p "bdd___bdd_poslit_p") :int32
  (f :pointer))

;;; BDD_NEGLIT_P (f)
;;; int bdd_neglit_p (BDDPTR f)
(cffi:defcfun (bdd_neglit_p "bdd___bdd_neglit_p") :int32
  (f :pointer))

;;; BDD_COFACTOR_POS (f)
;;; BDDPTR bdd_cofactor_pos_ (BDDPTR f)
(cffi:defcfun (bdd_cofactor_pos_ "bdd___bdd_cofactor_pos_") :pointer
  (f :pointer))

;;; BDD_COFACTOR_NEG (f)
;;; BDDPTR bdd_cofactor_neg_
(cffi:defcfun (bdd_cofactor_neg_ "bdd___bdd_cofactor_neg_") :pointer
  (f :pointer))

;;; void bdd_reset_marks (BDDPTR f)
;;; void bdd_traverse_pre (register BDDPTR v, void (*pre_action)(BDDPTR))
;;; void bdd_traverse_post (register BDDPTR v, void (*post_action)(BDDPTR))

;;; int bdd_size (BDDPTR f)
(cffi:defcfun (BDD_bdd_size "bdd___BDD_bdd_size") :int32
  (f :pointer))

;;; int bdd_size_vec (BDDPTR *f_vec, int size)
;;; int bdd_size_ceil (BDDPTR f, int ceiling)

;;; void bdd_init (void)
(cffi:defcfun (BDD_bdd_init "bdd___BDD_bdd_init") :void
  )

;;; void bdd_free (BDDPTR f)
(cffi:defcfun (bdd_free "bdd___bdd_free") :void
  (f :pointer))

;;; int bdd_gc (void)
(cffi:defcfun (bdd_gc "bdd___bdd_gc") :int32
  )

;;; BDDPTR bdd_ite (BDDPTR F, BDDPTR G, BDDPTR H)
(cffi:defcfun (bdd_ite "bdd___bdd_ite") :pointer
  (f :pointer)
  (g :pointer)
  (h :pointer))

;;; BDDPTR bdd_ite_const (BDDPTR F, BDDPTR G, BDDPTR H)
(cffi:defcfun (bdd_ite_const "bdd___bdd_ite_const") :pointer
  (f :pointer)
  (g :pointer)
  (h :pointer))

;;; void bdd_cofactors (BDDPTR f, BDDPTR *vp, BDDPTR *Tp, BDDPTR *Ep)
;;; BDDPTR bdd_invert_input_top (BDDPTR f)
(cffi:defcfun (bdd_invert_input_top "bdd___bdd_invert_input_top") :pointer
  (f :pointer))

;;; BDDPTR bdd_create_var (int v)
(cffi:defcfun (bdd_create_var "bdd___bdd_create_var") :pointer
  (v :int32))

;;; BDDPTR bdd_create_var_first	(void)
(cffi:defcfun (bdd_create_var_first "bdd___bdd_create_var_first") :pointer
  )

;;; BDDPTR bdd_create_var_before (BDDPTR v)
(cffi:defcfun (bdd_create_var_before "bdd___bdd_create_var_before") :pointer
  (v :pointer))

;;; BDDPTR bdd_create_var_after	(BDDPTR v)
(cffi:defcfun (bdd_create_var_after "bdd___bdd_create_var_after") :pointer
  (v :pointer))

;;; BDDPTR bdd_create_var_last (void)
(cffi:defcfun (bdd_create_var_last "bdd___bdd_create_var_last") :pointer
  )

;;; void bdd_print (FILE *fp, BDDPTR f, char *s)
(cffi:defcfun (bdd_print "bdd___bdd_print") :void
  (fp :pointer)
  (f :pointer)
  (s :string))

;;; void bdd_print_stats (FILE *fp)
;;; void bdd_quit (void)
(cffi:defcfun (bdd_quit "bdd___bdd_quit") :void
  )

;;; int bdd_memsize (void)
;;; int bdd_memsize_limit (void)
;;; void bdd_set_memsize_limit_and_handler (int limit, void (*handler) (void))
;;; int bdd_nodes_alive (void)
(cffi:defcfun (bdd_nodes_alive "bdd___bdd_nodes_alive") :int32
  )

;;; int bdd_nodes_allocated (void)
(cffi:defcfun (bdd_nodes_allocated "bdd___bdd_nodes_allocated") :int32
  )

;;; int bdd_nr_occurs_var (int id)
;;; int bdd_compl_p (BDDPTR f, BDDPTR g)
;;; int bdd_equal_p (BDDPTR F, BDDPTR G)
(cffi:defcfun (bdd_equal_p "bdd___bdd_equal_p") :int32
  (f :pointer)
  (g :pointer))

;;; int bdd_implies_taut (BDDPTR F, BDDPTR G)
;;; BDDPTR bdd_not (BDDPTR F)
(cffi:defcfun (bdd_not "bdd___bdd_not") :pointer
  (f :pointer))

;;; BDDPTR bdd_and (BDDPTR F, BDDPTR G)
(cffi:defcfun (bdd_and "bdd___bdd_and") :pointer
  (f :pointer)
  (g :pointer))

;;; BDDPTR bdd_greater	(BDDPTR F, BDDPTR G)
(cffi:defcfun (bdd_greater "bdd___bdd_greater") :pointer
  (f :pointer)
  (g :pointer))

;;; BDDPTR bdd_less (BDDPTR F, BDDPTR G)
(cffi:defcfun (bdd_less "bdd___bdd_less") :pointer
  (f :pointer)
  (g :pointer))

;;; BDDPTR bdd_xor (BDDPTR F, BDDPTR G)
(cffi:defcfun (bdd_xor "bdd___bdd_xor") :pointer
  (f :pointer)
  (g :pointer))

;;; BDDPTR bdd_or (BDDPTR F, BDDPTR G)
(cffi:defcfun (bdd_or "bdd___bdd_or") :pointer
  (f :pointer)
  (g :pointer))

;;; BDDPTR bdd_nor (BDDPTR F, BDDPTR G)
(cffi:defcfun (bdd_nor "bdd___bdd_nor") :pointer
  (f :pointer)
  (g :pointer))

;;; BDDPTR bdd_equiv (BDDPTR F, BDDPTR G)
(cffi:defcfun (bdd_equiv "bdd___bdd_equiv") :pointer
  (f :pointer)
  (g :pointer))

;;; BDDPTR bdd_xnor (BDDPTR F, BDDPTR G) /* equivalent to bdd_equiv */
(cffi:defcfun (bdd_xnor "bdd___bdd_xnor") :pointer
  (f :pointer)
  (g :pointer))

;;; BDDPTR bdd_implied (BDDPTR F, BDDPTR G)
(cffi:defcfun (bdd_implied "bdd___bdd_implied") :pointer
  (f :pointer)
  (g :pointer))

;;; BDDPTR bdd_implies (BDDPTR F, BDDPTR G)
(cffi:defcfun (bdd_implies "bdd___bdd_implies") :pointer
  (f :pointer)
  (g :pointer))

;;; BDDPTR bdd_nand (BDDPTR F, BDDPTR G)
(cffi:defcfun (bdd_nand "bdd___bdd_nand") :pointer
  (f :pointer)
  (g :pointer))

;;; BDDPTR bdd_0 (void)
(cffi:defcfun (bdd_0 "bdd___bdd_0") :pointer
  )

;;; BDDPTR bdd_1 (void)
(cffi:defcfun (bdd_1 "bdd___bdd_1") :pointer
  )

;;; BDDPTR bdd_X (void)
(cffi:defcfun (bdd_X "bdd___bdd_X") :pointer
  )

;;; BDDPTR bdd_assign (BDDPTR f)
(cffi:defcfun (bdd_assign "bdd___bdd_assign") :pointer
  (f :pointer))

;;; BDDPTR bdd_top_var (BDDPTR f)
;;; int bdd_top_var_rank (BDDPTR f)
;;; BDDPTR bdd_then (BDDPTR f)
(cffi:defcfun (BDD_bdd_then "bdd___BDD_bdd_then") :pointer
  (f :pointer))

;;; BDDPTR bdd_else (BDDPTR f)
(cffi:defcfun (BDD_bdd_else "bdd___BDD_bdd_else") :pointer
  (f :pointer))

;;; BDDPTR bdd_apply (BDDPTR (*f)(BDDPTR,BDDPTR),BDDPTR a,BDDPTR b)
(cffi:defcfun (bdd_apply "bdd___bdd_apply") :pointer
  (f :pointer)
  (a :pointer)
  (b :pointer))

;;; BDDPTR bdd_constrain (BDDPTR f, BDDPTR c)
(cffi:defcfun (bdd_constrain "bdd___bdd_constrain") :pointer
  (f :pointer)
  (c :pointer))

;;; BDDPTR bdd_top_var (BDDPTR f)
(cffi:defcfun (bdd_top_var "bdd___bdd_top_var") :pointer
  (f :pointer))

;;; BDD_LIST bdd_sum_of_cubes (BDDPTR f, int irredundant)
(cffi:defcfun (bdd_sum_of_cubes "bdd___bdd_sum_of_cubes") :pointer
  (f :pointer)
  (irredundant :int32))

;;; The following were obtained by looking through mu.c and collecting
;;; functions not mentioned above.

;;; int bdd_reorder_var (int var_id, int target_var_id)
(cffi:defcfun (bdd_reorder_var "bdd___bdd_reorder_var") :int32
  (var_id :int32)
  (target_var_id :int32))

;;; BDDPTR bdd_and_smooth (BDDPTR f, BDDPTR g, BDD_LIST vars)
(cffi:defcfun (bdd_and_smooth "bdd___bdd_and_smooth") :pointer
  (f :pointer)
  (g :pointer)
  (vars :pointer))

;;; BDD_LIST bdd_rank_order_vars (BDD_LIST vars)
(cffi:defcfun (bdd_rank_order_vars "bdd___bdd_rank_order_vars") :pointer
  (vars :pointer))

;;; BDDPTR bdd_quantify (int existential, BDDPTR f, BDD_LIST vars)
(cffi:defcfun (bdd_quantify "bdd___bdd_quantify") :pointer
  (existential :int32)
  (f :pointer)
  (vars :pointer))

;;; BDDPTR bdd_subst_par (BDDPTR *f_vec, BDD_LIST vars, BDDPTR g)
(cffi:defcfun (bdd_subst_par "bdd___bdd_subst_par") :pointer
  (f_vec :pointer)
  (vars :pointer)
  (g :pointer))

;;; BDDPTR bdd_subst_par_list (BDD_LIST f_list, BDD_LIST vars, BDDPTR g)
(cffi:defcfun (bdd_subst_par_list "bdd___bdd_subst_par_list") :pointer
  (f_list :pointer)
  (vars :pointer)
  (g :pointer))

;;; void bdd_free_vec (BDDPTR *f_vec, int size)
(cffi:defcfun (bdd_free_vec "bdd___bdd_free_vec") :void
  (f_vec :pointer)
  (size :int32))

;;; const char *bdd_get_output_string (int idx)
(cffi:defcfun (bdd_get_output_string "bdd___bdd_get_output_string") :string
  (idx :int32))

;;; void bdd_set_output_string (int idx, const char *str)
(cffi:defcfun (bdd_set_output_string "bdd___bdd_set_output_string") :void
  (idx :int32)
  (str :string))

;;; void bdd_print_as_sum_of_cubes (FILE *fp, BDDPTR f, int irredundant)
(cffi:defcfun (bdd_print_as_sum_of_cubes "bdd___bdd_print_as_sum_of_cubes") :void
  (fp :pointer)
  (f :pointer)
  (irredundant :int32))

;;; BDDPTR bdd_diff (BDDPTR f, BDD_LIST vars)
(cffi:defcfun (bdd_diff "bdd___bdd_diff") :pointer
  (f :pointer)
  (vars :pointer))

;;; BDDPTR bdd_one_of_vec (BDDPTR *vec, int size)
(cffi:defcfun (bdd_one_of_vec "bdd___bdd_one_of_vec") :pointer
  (vec :pointer)
  (size :int32))

;;; BDDPTR bdd_none_of_vec (BDDPTR *args, int size)
(cffi:defcfun (bdd_none_of_vec "bdd___bdd_none_of_vec") :pointer
  (args :pointer)
  (size :int32))

;;; BDDPTR bdd_subst (BDDPTR f, int var, BDDPTR g)
(cffi:defcfun (bdd_subst "bdd___bdd_subst") :pointer
  (f :pointer)
  (var :int32)
  (g :pointer))

;;; BDD_LIST bdd_sum_of_cubes_as_list (BDDPTR f)
(cffi:defcfun (bdd_sum_of_cubes_as_list "bdd___bdd_sum_of_cubes_as_list") :pointer
  (f :pointer))

;;; int bdd_traverse_cube (BDDPTR cube,
;;;                        void (*action) (int index, int neg, int first))
(cffi:defcfun (bdd_traverse_cube "bdd___bdd_traverse_cube") :int32
  (cube :pointer)
  (action :pointer))

;;; BDD_LIST bdd_support_as_list_of_vars (BDDPTR f)
(cffi:defcfun (bdd_support_as_list_of_vars "bdd___bdd_support_as_list_of_vars") :pointer
  (f :pointer))

(defun bdd-interrupted? ()
  ;; Must eval, or allegro breaks on signal 10 (bus error)
  (not (zerop (eval 'bdd_interrupted))))

;; (eval-when (:execute :load-toplevel)
;;   (BDD_bdd_init))
