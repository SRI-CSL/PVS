/* dfa.c */

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
#include "bdd.h"
#include "../BDD/hash.h"

DFA *dfaMake(int n)
{
  DFA *a;

  a = mem_get_block(sizeof *a);
  a->bddm = bdd_new_manager(8 * n, ((n+3)/4)*4 ); /* overflow_increment rounded
						     up to be div. by 4 */
  a->ns = n;
  a->q = mem_get_block((sizeof *(a->q)) * n);
  a->f = mem_get_block((sizeof *(a->f)) * n); 
 
  return a;
}

DFA *dfaMakeNoBddm(int n)
{
  DFA *a;

  a = mem_get_block(sizeof *a);
  a->ns = n;
  a->q = mem_get_block((sizeof *(a->q)) * n);
  a->f = mem_get_block((sizeof *(a->f)) * n); 

  return a;
}

void dfaFree(DFA *a) 
{ 
  bdd_kill_manager(a->bddm);
  mem_free_block(a->q);
  mem_free_block(a->f);
  mem_free_block(a);
}

void dfaNegation(DFA *a) 
{  
  int i;

  for(i=0; i<(a->ns); i++) 
    a->f[i] =  -(a->f[i]); 
}

 
void dfaRestrict(DFA *a) 
{  
  int i;

  for(i=0; i<(a->ns); i++)
    if (a->f[i] == -1)
      a->f[i] = 0;
}

void dfaUnrestrict(DFA *a) 
{  
  int i;

  for(i=0; i<(a->ns); i++)
    if (a->f[i] == 0)
      a->f[i] = -1;
}

/** 
void dfaPrintStatistics() 
{
  bdd_print_statistics((unsigned)MINIMIZATION, "Minimization");
  bdd_print_statistics((unsigned)PRODUCT, "Product");
  bdd_print_statistics((unsigned)PROJECT, "Project");
} 
**/

DFA *dfaCopy(DFA *a)
{
  unsigned i;

  DFA * result = dfaMake(a->ns);
  result->ns = a->ns;
  result->s = a->s;
  mem_copy(result->f, a->f, sizeof(*a->f)*a->ns);
  
  bdd_prepare_apply1(a->bddm);

  for (i = 0; i < a->ns; i++)
    (void) bdd_apply1(a->bddm, a->q[i], result->bddm, &fn_identity);
  
  mem_copy(result->q, bdd_roots(result->bddm), sizeof(bdd_ptr)*a->ns);

  return result;
}

void dfaReplaceIndices(DFA *a, int *indices_map)
{
  unsigned i;

  bdd_prepare_apply1(a->bddm);

  for (i = 0; i < a->ns; i++)
    bdd_replace_indices(a->bddm, a->q[i], indices_map);
}
