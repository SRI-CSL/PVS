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
#include "bdd.h"
#include "../BDD/hash.h"
#include "../Mem/mem.h"

int dfa_in_mem; /* number of automata currently in memory */

DFA *dfaMake(int n)
{
  DFA *a;

  a = mem_alloc(sizeof *a);
  a->bddm = bdd_new_manager(8 * n, ((n+3)/4)*4 ); /* overflow_increment rounded
						     up to be div. by 4 */
  a->ns = n;
  a->q = mem_alloc((sizeof *(a->q)) * n);
  a->f = mem_alloc((sizeof *(a->f)) * n); 
 
  dfa_in_mem++;
  return a;
}

DFA *dfaMakeNoBddm(int n)
{
  DFA *a;

  a = mem_alloc(sizeof *a);
  a->ns = n;
  a->q = mem_alloc((sizeof *(a->q)) * n);
  a->f = mem_alloc((sizeof *(a->f)) * n); 

  dfa_in_mem++;
  return a;
}

void dfaFree(DFA *a) 
{ 
  bdd_kill_manager(a->bddm);
  mem_free(a->q);
  mem_free(a->f);
  mem_free(a);
  dfa_in_mem--;
}

void dfaNegation(DFA *a) 
{  
  int i;
  for (i = 0; i < a->ns; i++) 
    a->f[i] = - a->f[i]; 
}

 
void dfaRestrict(DFA *a) 
{  
  int i;
  for (i = 0; i < a->ns; i++)
    if (a->f[i] == -1)
      a->f[i] = 0;
}

void dfaUnrestrict(DFA *a) 
{  
  int i;
  for (i = 0; i < a->ns; i++)
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
  int i;

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
  int i;

  bdd_prepare_apply1(a->bddm);

  for (i = 0; i < a->ns; i++)
    bdd_replace_indices(a->bddm, a->q[i], (unsigned int *) indices_map);
}
