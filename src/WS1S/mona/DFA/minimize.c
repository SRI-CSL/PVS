/* minimize.c */

/*
 * MONA is Copyright (C) 1997 BRICS. All rights reserved. 
 *
 * Reproduction of all or part of this software is permitted for
 * educational or research use on condition that this copyright notice is
 * included in any copy. This software comes with no warranty of any
 * kind. In no event will BRICS be liable for any damages resulting from
 * use of this software.
 */

#include "dfa.h"
#include "../BDD/hash.h"

/* used by minimization_term_fn */
static int *final;
static unsigned *discrs;
static unsigned length;

static bdd_ptr minimization_term_fn(bdd_ptr p)
{
  return (discrs[p]);
}

 
static unsigned rename_partition(unsigned *roots)
/* calculate equivalence classes as given by the conjunction of bdd_roots 
and final; put the result in discrs and return the number of classes*/
{  
  hash_tab htbl = new_hash_tab(&hash2, &eq2);
  unsigned next = 0;
  unsigned i;
  
  for (i = 0;  i < length; i++) {
    unsigned k = (unsigned) lookup_in_hash_tab(htbl, (unsigned)roots[i], final[i]);

    if (k == 0) {
      insert_in_hash_tab(htbl, 
			 (unsigned)roots[i], final[i],
			 (pointer) ++next);
      discrs[i] = next - 1;
    }
    else
      discrs[i] = k - 1;
  };
  free_hash_tab(htbl);

  return(next);
}


DFA *dfaMinimize(DFA *a) 
{
  unsigned num_old_blocs;
  unsigned num_new_blocs = 2;
  unsigned i;
  bdd_manager *bddm = a->bddm;
  bdd_manager *new_bddm = 0;
  unsigned not_first = 0;

  length = a->ns;
  final = a->f;
  
  discrs = mem_get_block((sizeof *discrs) * length);
  
  {
    unsigned *roots =  mem_get_block((SIZE_T)(sizeof *roots) * length);
    mem_zero(roots,(SIZE_T)(sizeof *roots) * length);
    rename_partition(roots);
    mem_free_block(roots);
  }
  
  do {

    if (not_first) {
      bdd_update_statistics(new_bddm, (unsigned)MINIMIZATION);
      bdd_kill_manager(new_bddm);
    }  
    else 
      not_first = 1;
    
    new_bddm = bdd_new_manager(bddm->table_elements, 
			       bddm->table_elements/8 + 4);
    bdd_prepare_apply1(bddm);
    
    for (i = 0; i < length; i++)
	(void) bdd_apply1(bddm, a->q[i], new_bddm, &minimization_term_fn);
    
    num_old_blocs = num_new_blocs;
    num_new_blocs = rename_partition(bdd_roots(new_bddm));

  } while (num_new_blocs > num_old_blocs);
  
  {
    DFA *b = dfaMakeNoBddm(num_new_blocs);
    unsigned *roots = bdd_roots(new_bddm);

    b->bddm = new_bddm;
    for (i = 0; i < length; i++) {
      b->q[discrs[i]]  = roots[i];
      b->f[discrs[i]]  = final[i];
    }
    b->s = discrs[a->s];
    
    bdd_update_statistics(new_bddm, (unsigned)MINIMIZATION);
    mem_free_block(discrs);
    return (b);
  }
}
