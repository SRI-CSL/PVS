/*
 DOCUMENTATION INFORMATION				         module: BDD
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system	   : Apollo DN3000, HP 9000/S500, IBM RS/6000
 file	   : bdd_fns.h
 unit-title: 
 ref.	   : Efficient Implementation of a BDD Package, Karl S. Brace. DAC'90
 author(s) : Copyright (c) 1990-1996 G.L.J.M. Janssen
 date	   : 23-APR-1996
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

/* ************************************************************************ */
/* FILE DOCUMENTATION:                                                      */
/*                                                                          */
/* ************************************************************************ */

#ifndef BDD_FNS_H
#define BDD_FNS_H

#include "bdd.h"
#include "bdd_extern.h"
#include "bdd_quant.h"

/* ------------------------------------------------------------------------ */
/* DEFINES                                                                  */
/* ------------------------------------------------------------------------ */

#define FALSE		0
#define TRUE		1
#define DONTCARE	2

#define FOR_EACH_CUBE(list, c) \
  { \
    BDD_LIST _xyz_list = list; \
    register BDD_ELEM _xyz_p; \
    register BDDPTR c; \
    \
    if (_xyz_list) \
      for (_xyz_p = BDD_LIST_FIRST (_xyz_list); \
           _xyz_p; \
           _xyz_p = BDD_LIST_NEXT (_xyz_p)) {\
	c = (BDDPTR) BDD_ELEM_CONTENTS (_xyz_p);

#define END_FOR_EACH_CUBE	}}

#define bdd_cofactor_pos(f, var)	bdd_subst (BDD_1, var, f)
#define bdd_cofactor_neg(f, var)	bdd_subst (BDD_0, var, f)
#define bdd_cofactor_pos_vec(F,i,j,var) bdd_subst_vec (F, i, j, var, BDD_1)
#define bdd_cofactor_neg_vec(F,i,j,var) bdd_subst_vec (F, i, j, var, BDD_0)

/* ------------------------------------------------------------------------ */
/* TYPE DEFINITIONS                                                         */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* VARIABLES                                                                */
/* ------------------------------------------------------------------------ */
static char SccsId_BDD_FNS_H[] = "%Z%%Y%/%M% %I% %G%";

/* ------------------------------------------------------------------------ */
/* FUNCTION PROTOTYPES                                                      */
/* ------------------------------------------------------------------------ */

extern      int bdd_sat                        (BDDPTR f, BYTE *pi);
/*
extern      int bdd_count_sat_assignments      (BDDPTR f);
extern   double bdd_count_sat_assignments      (BDDPTR f);
extern   Double bdd_count_sat_assignments      (BDDPTR f);
*/
extern   Double bdd_count_sat_assignments      (BDDPTR f, BDDPTR domain);
extern   Double bdd_count_X_terms              (BDDPTR f);
extern      int bdd_in_support                 (int var, BDDPTR f);
extern   BDDPTR bdd_list_of_vars_as_cube       (BDD_LIST vars);
extern BDD_LIST bdd_support_as_list_of_vars    (BDDPTR f);
extern   BDDPTR bdd_support_as_cube            (BDDPTR f);
extern      int bdd_nr_support_vars            (BDDPTR f);
extern BDD_LIST bdd_cube_as_list_of_vars       (BDDPTR cube);
extern BDD_LIST bdd_rank_order_vars	       (BDD_LIST vars);
extern   BDDPTR bdd_subst                      (BDDPTR f, int var, BDDPTR g);
extern   BDDPTR *bdd_subst_vec  (BDDPTR *F, int i, int j, int var, BDDPTR g);
/*
extern   BDDPTR bdd_subst2                     (BDDPTR h, BDDPTR g, BDDPTR f);
*/
extern   BDDPTR bdd_subst_par (BDDPTR *f_vec, BDD_LIST vars, BDDPTR g);
extern   BDDPTR bdd_compose                    (BDDPTR f, BDDPTR g, BDDPTR h);
extern      int bdd_cube_p                     (BDDPTR f);
extern      int bdd_size_cube                  (BDDPTR cube);
extern      int bdd_has_dontcare               (BDDPTR f);
extern   BDDPTR bdd_on_set                     (BDDPTR f);
extern   BDDPTR bdd_off_set                    (BDDPTR f);
extern   BDDPTR bdd_dontcare_set               (BDDPTR f);
extern   BDDPTR bdd_replace_X		       (BDDPTR f, BDDPTR X_is);
extern   BDDPTR bdd_minimize_dontcares         (BDDPTR f);
extern   BDDPTR *bdd_minimize_dontcares_vec    (BDDPTR *f_vec, int size);
extern   BDDPTR bdd_none_of_list               (BDD_LIST args);
extern   BDDPTR bdd_none_of_vec                (BDDPTR *args, int size);
extern   BDDPTR bdd_one_of_list                (BDD_LIST args);
extern   BDDPTR bdd_one_of_vec                 (BDDPTR *vec, int size);
extern   BDDPTR bdd_diff                       (BDDPTR f, BDD_LIST vars);
extern BDD_LIST bdd_shortest_path_to_1_as_list (BDDPTR f);
extern   BDDPTR bdd_shortest_path_as_cube      (BDDPTR f, int to_1, int *len);
extern   BDDPTR bdd_shortest_path_to_1_as_cube (BDDPTR f);
extern      int bdd_unate_in                   (BDDPTR f, int var);
extern BDD_LIST bdd_sum_of_cubes_as_list       (BDDPTR f);
extern BDD_LIST bdd_irredundant_sum_of_cubes_as_list (BDDPTR f);
extern      int bdd_traverse_cube
  		(BDDPTR cube, void (* action) (int index, int neg, int first));
extern   BDDPTR bdd_prime_implicant	       (BDDPTR f);
extern   BDDPTR bdd_eval                       (BDDPTR f, BDDPTR cube);
extern   BDDPTR bdd_invert_input_interpret_mod_bits (BDDPTR f);
extern   BDDPTR bdd_invert_input               (BDDPTR f, int var);
extern   BDDPTR bdd_invert_inputs              (BDDPTR f);
extern     void bdd_use_sop_cache_switch       (int on);
extern     void bdd_cleanup_sop_cache          (void);
/*
extern      int bdd_subfunction_p              (BDDPTR g, BDDPTR f);
*/
extern   BDDPTR bdd_cube_factor                (BDDPTR f);

#endif /* BDD_FNS_H */
