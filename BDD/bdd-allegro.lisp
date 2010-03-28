;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bdd-allegro.lisp -- Interface to the BDD package
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

;;; int null_list_p (LIST x)
(ff:def-foreign-call (null_list_p "bdd___null_list_p")
    ((x :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
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
(ff:def-foreign-call (BDD_bdd_size "bdd___BDD_bdd_size")
    ((f :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)

;;; int bdd_size_vec (BDDPTR *f_vec, int size)
;;; int bdd_size_ceil (BDDPTR f, int ceiling)

;;; void bdd_init (void)
(ff:def-foreign-call (BDD_bdd_init "bdd___BDD_bdd_init")
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
(ff:def-foreign-call (BDD_bdd_then "bdd___BDD_bdd_then")
    ((f :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; BDDPTR bdd_else (BDDPTR f)
(ff:def-foreign-call (BDD_bdd_else "bdd___BDD_bdd_else")
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

(defun bdd-interrupted? ()
  ;; Must eval, or allegro breaks on signal 10 (bus error)
  (not (zerop (eval 'bdd_interrupted))))

(eval-when (eval load)
  (BDD_bdd_init))
