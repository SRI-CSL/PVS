#include <stdio.h>

#include "bdd_fns.h"

int debug = 0;
int warnings = 1;

/* Make list macros available */
null_list_p (LIST x) { return x == NULL; }
void *elem_contents (LIST_ELEM_PTR x) { return ELEM_CONTENTS (x); }
LIST_ELEM_PTR list_first (LIST x) { return LIST_FIRST (x); }
LIST_ELEM_PTR list_last (LIST x) { return LIST_LAST (x); }
list_info (LIST x) { return LIST_INFO (x); }
LIST_ELEM_PTR list_next (LIST_ELEM_PTR x) { return LIST_NEXT (x); }

/* Provide a means for setting global variables */
set_bdd_do_gc (int flag) { bdd_do_gc = flag; }
set_bdd_do_dynamic_ordering (int flag) { bdd_do_dynamic_ordering = flag; }
set_bdd_verbose (int flag) { bdd_verbose = flag; }
set_bdd_use_neg_edges (int flag) { bdd_use_neg_edges = flag; }
set_bdd_use_inv_edges (int flag) { bdd_use_inv_edges = flag; }


bdd_varid (BDDPTR f) { return BDD_VARID (f); }

bdd_0_p (BDDPTR f) { return BDD_0_P (f); }
bdd_1_p (BDDPTR f) { return BDD_1_P (f); }
bdd_x_p (BDDPTR f) { return BDD_X_P (f); }
bdd_term_p (BDDPTR f) { return BDD_TERM_P (f); }
bdd_lit_p (BDDPTR f) { return BDD_LIT_P (f); }
BDDPTR bdd_cofactor_pos_ (BDDPTR f) { return BDD_COFACTOR_POS (f); }
BDDPTR bdd_cofactor_neg_ (BDDPTR f) { return BDD_COFACTOR_NEG (f); }


BDD_LIST bdd_sum_of_cubes (BDDPTR f, int irredundant)
{
  BDD_LIST foo;
  
  if (BDD_VOID_P (f))
    return NULL_LIST;

  if (irredundant) {
    foo = foobarsucks(f); /* bdd_irredundant_sum_of_cubes_as_list (f);*/
    return foo;
  }
  return bdd_sum_of_cubes_as_list(f);
  
/*     return irredundant ? bdd_irredundant_sum_of_cubes_as_list (f)
	:             bdd_sum_of_cubes_as_list (f); */
  
}

