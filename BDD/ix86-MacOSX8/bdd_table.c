#include "bdd_fns.h"

BDDPTR bdd___bdd_top_var (BDDPTR f) {return bdd_top_var (f);}

int bdd___bdd_poslit_p (BDDPTR f) {return bdd_poslit_p (f);}

int bdd___bdd_neglit_p (BDDPTR f) {return bdd_neglit_p (f);}

int bdd___bdd_equal_p (BDDPTR F, BDDPTR G) {return bdd_equal_p (F, G);}

void bdd___elem_contents (LIST_ELEM_PTR x) {elem_contents (x);}

LIST_ELEM_PTR bdd___list_first (LIST x) {return list_first (x);}

LIST_ELEM_PTR bdd___list_last (LIST x) {return list_last (x);}

void bdd___list_info (LIST x) {list_info (x);}

LIST_ELEM_PTR bdd___list_next (LIST_ELEM_PTR x) {return list_next (x);}

void bdd___set_bdd_do_gc (int flag) {set_bdd_do_gc (flag);}

void bdd___set_bdd_do_dynamic_ordering (int flag)
  {set_bdd_do_dynamic_ordering (flag);}

void bdd___set_bdd_verbose (int flag) {set_bdd_verbose (flag);}

void bdd___set_bdd_use_neg_edges (int flag)
  {set_bdd_use_neg_edges (flag);}

void bdd___set_bdd_use_inv_edges (int flag)
  {set_bdd_use_inv_edges (flag);}

int bdd___bdd_varid (BDDPTR f) {bdd_varid (f);}

int bdd___bdd_void_p (BDDPTR f) {return bdd_void_p (f);}

int bdd___bdd_1_p (BDDPTR f) {return bdd_1_p (f);}

int bdd___bdd_0_p (BDDPTR f) {return bdd_0_p (f);}

int bdd___bdd_x_p (BDDPTR f) {return bdd_x_p (f);}

int bdd___bdd_const_p (BDDPTR f) {return bdd_const_p (f);}

int bdd___bdd_term_p (BDDPTR f) {return bdd_term_p (f);}

int bdd___bdd_lit_p (BDDPTR f) {return bdd_lit_p (f);}

BDDPTR bdd___bdd_cofactor_pos_ (BDDPTR f) {return bdd_cofactor_pos_ (f);}

BDDPTR bdd___bdd_cofactor_neg_ (BDDPTR f) {return bdd_cofactor_neg_ (f);}

int bdd___bdd_size (BDDPTR f) {return bdd_size (f);}

void bdd___bdd_init (void) {bdd_init ();}

void bdd___bdd_free (BDDPTR f) {bdd_free (f);}

int bdd___bdd_gc (void) {return bdd_gc ();}

BDDPTR bdd___bdd_ite (BDDPTR F, BDDPTR G, BDDPTR H) {return bdd_ite (F, G, H);}

BDDPTR bdd___bdd_ite_const (BDDPTR F, BDDPTR G, BDDPTR H)
  {return bdd_ite_const (F, G, H);}

BDDPTR bdd___bdd_invert_input_top (BDDPTR f) {return bdd_invert_input_top (f);}

BDDPTR bdd___bdd_create_var (int v) {return bdd_create_var (v);}

BDDPTR bdd___bdd_create_var_first (void) {return bdd_create_var_first ();}

BDDPTR bdd___bdd_create_var_before (BDDPTR v)
  {return bdd_create_var_before (v);}

BDDPTR bdd___bdd_create_var_after (BDDPTR v) {return bdd_create_var_after (v);}

BDDPTR bdd___bdd_create_var_last (void) {return bdd_create_var_last ();}

void bdd___bdd_print (FILE *fp, BDDPTR f, char *s)
  {return bdd_print (fp, f, s);}

void bdd___bdd_quit (void) {return bdd_quit ();}

int bdd___bdd_nodes_alive (void) {return bdd_nodes_alive ();}

int bdd___bdd_nodes_allocated (void) {return bdd_nodes_allocated ();}

BDDPTR bdd___bdd_not (BDDPTR F) {return bdd_not (F);}

BDDPTR bdd___bdd_and (BDDPTR F, BDDPTR G) {return bdd_and (F, G);}

BDDPTR bdd___bdd_greater (BDDPTR F, BDDPTR G) {return bdd_greater (F, G);}

BDDPTR bdd___bdd_less (BDDPTR F, BDDPTR G) {return bdd_less (F, G);}

BDDPTR bdd___bdd_xor (BDDPTR F, BDDPTR G) {return bdd_xor (F, G);}

BDDPTR bdd___bdd_or (BDDPTR F, BDDPTR G) {return bdd_or (F, G);}

BDDPTR bdd___bdd_nor (BDDPTR F, BDDPTR G) {return bdd_nor (F, G);}

BDDPTR bdd___bdd_equiv (BDDPTR F, BDDPTR G) {return bdd_equiv (F, G);}

BDDPTR bdd___bdd_xnor (BDDPTR F, BDDPTR G) {return bdd_xnor (F, G);}

BDDPTR bdd___bdd_implied (BDDPTR F, BDDPTR G) {return bdd_implied (F, G);}

BDDPTR bdd___bdd_implies (BDDPTR F, BDDPTR G) {return bdd_implies (F, G);}

BDDPTR bdd___bdd_nand (BDDPTR F, BDDPTR G) {return bdd_nand (F, G);}

BDDPTR bdd___bdd_0 (void) {return bdd_0 ();}

BDDPTR bdd___bdd_1 (void) {return bdd_1 ();}

BDDPTR bdd___bdd_X (void) {return bdd_X ();}

BDDPTR bdd___bdd_assign (BDDPTR f) {return bdd_assign (f);}

BDDPTR bdd___bdd_then (BDDPTR f) {return bdd_then (f);}

BDDPTR bdd___bdd_else (BDDPTR f) {return bdd_else (f);}

BDDPTR bdd___bdd_apply (BDDPTR (*f)(BDDPTR,BDDPTR), BDDPTR a, BDDPTR b)
  {return bdd_apply (f, a, b);}

BDDPTR bdd___bdd_constrain (BDDPTR f, BDDPTR c) {return bdd_constrain (f, c);}

int bdd___bdd_reorder_var (int var_id, int target_var_id)
  {return bdd_reorder_var (var_id, target_var_id);}

BDDPTR bdd___bdd_and_smooth (BDDPTR f, BDDPTR g, BDD_LIST vars)
  {return bdd_and_smooth (f, g, vars);}

BDD_LIST bdd___bdd_rank_order_vars (BDD_LIST vars)
  {return bdd_rank_order_vars (vars);}

BDDPTR bdd___bdd_quantify (int existential, BDDPTR f, BDD_LIST vars)
  {return bdd_quantify (existential, f, vars);}

BDDPTR bdd___bdd_subst_par (BDDPTR *f_vec, BDD_LIST vars, BDDPTR g)
  {return bdd_subst_par (f_vec, vars, g);}

BDDPTR bdd___bdd_subst_par_list (BDD_LIST f_list, BDD_LIST vars, BDDPTR g)
  {return bdd_subst_par_list (f_list, vars, g);}

void bdd___bdd_free_vec (BDDPTR *f_vec, int size)
  {return bdd_free_vec (f_vec, size);}

void bdd___bdd_set_output_string (int idx, const char *str)
  {return bdd_set_output_string (idx, str);}

void bdd___bdd_print_as_sum_of_cubes (FILE *fp, BDDPTR f, int irredundant)
  {return bdd_print_as_sum_of_cubes (fp, f, irredundant);}

BDDPTR bdd___bdd_diff (BDDPTR f, BDD_LIST vars) {return bdd_diff (f, vars);}

BDDPTR bdd___bdd_one_of_vec (BDDPTR *vec, int size)
  {return bdd_one_of_vec (vec, size);}

BDDPTR bdd___bdd_none_of_vec (BDDPTR *args, int size)
  {return bdd_none_of_vec (args, size);}

BDDPTR bdd___bdd_subst (BDDPTR f, int var, BDDPTR g)
  {return bdd_subst (f, var, g);}

BDD_LIST bdd___bdd_sum_of_cubes_as_list (BDDPTR f)
  {return bdd_sum_of_cubes_as_list (f);}

int bdd___bdd_traverse_cube (BDDPTR cube,
			     void (*action) (int index, int neg, int first))
  {return bdd_traverse_cube (cube, action);}

BDD_LIST bdd___bdd_support_as_list_of_vars (BDDPTR f)
  {return bdd_support_as_list_of_vars (f);}

