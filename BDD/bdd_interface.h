#ifndef BDD_INTERFACE_H
#define BDD_INTERFACE_H

extern int null_list_p (LIST x);
extern void *elem_contents (LIST_ELEM_PTR x);
extern LIST_ELEM_PTR list_first (LIST x);
extern LIST_ELEM_PTR list_last (LIST x);
extern int list_info (LIST x);
extern LIST_ELEM_PTR list_next (LIST_ELEM_PTR x);
extern void set_bdd_do_gc (int flag);
extern void set_bdd_do_dynamic_ordering (int flag);
extern void set_bdd_verbose (int flag);
extern void set_bdd_use_neg_edges (int flag);
extern void set_bdd_use_inv_edges (int flag);
extern unsigned int bdd_varid (BDDPTR f);
extern int bdd_0_p (BDDPTR f);
extern int bdd_1_p (BDDPTR f);
extern int bdd_x_p (BDDPTR f);
extern int bdd_term_p (BDDPTR f);
extern int bdd_lit_p (BDDPTR f);
extern BDDPTR bdd_cofactor_pos_ (BDDPTR f);
extern BDDPTR bdd_cofactor_neg_ (BDDPTR f);
extern BDD_LIST bdd_sum_of_cubes (BDDPTR f, int irredundant);

#endif /* BDD_INTERFACE_H */
