/*
 * MONA
 * Copyright (C) 1997-2000 BRICS.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the  Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
 * USA.
 */

#include "dfa.h"
#include "../BDD/hash.h"
#include "../Mem/mem.h"

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
    unsigned k = (unsigned) (long)
      lookup_in_hash_tab(htbl, (unsigned)roots[i], final[i]);

    if (k == 0) {
      insert_in_hash_tab(htbl, 
			 (unsigned)roots[i], final[i],
			 (void *) (long) ++next);
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
  
  discrs = mem_alloc((sizeof *discrs) * length);
  
  {
    unsigned *roots =  mem_alloc((size_t)(sizeof *roots) * length);
    mem_zero(roots,(size_t)(sizeof *roots) * length);
    rename_partition(roots);
    mem_free(roots);
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
    mem_free(discrs);
    return (b);
  }
}
