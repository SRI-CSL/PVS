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

#include <stdlib.h>
#include "dfa.h"
#include "../Mem/mem.h"

static int **preds; /* preds[i] is the set of predecessors of i */
static int *predalloc, *predused; /* allocated/used size of preds[i] */

static int current_state;

void successors(bdd_manager *bddm, bdd_ptr p)
{
  if (bdd_is_leaf(bddm, p)) { 
    int i;
    int s = bdd_leaf_value(bddm, p); /* current_state is a predecessor of s */

    for (i = 0; i < predused[s]; i++) /* already there? */
      if (preds[s][i] == current_state)
	return;

    if (predalloc[s] == predused[s]) { /* need to reallocate? */
      predalloc[s] = predalloc[s]*2+8;
      preds[s] = (int *) mem_resize(preds[s], sizeof(int) * predalloc[s]);
    }

    preds[s][predused[s]++] = current_state;
  }
  else {
    successors(bddm, bdd_else(bddm, p));
    successors(bddm, bdd_then(bddm, p));
  }
  
}

void dfaPrefixClose(DFA *a)
{
  unsigned i;
  int *queue = (int *) mem_alloc(sizeof(int) * a->ns);
  int queueused = 0, next = 0;

  predalloc = (int *) mem_alloc(sizeof(int) * a->ns);
  predused = (int *) mem_alloc(sizeof(int) * a->ns);
  preds = (int **) mem_alloc(sizeof(int *) * a->ns);
  for (i = 0; i < a->ns; i++) {
    predalloc[i] = predused[i] = 0;
    preds[i] = 0;
  }

  /* find predecessor sets and initialize queue with final states */
  for (i = 0; i < a->ns; i++) {
    current_state = i;
    successors(a->bddm, a->q[i]);
    if (a->f[i] == 1)
      queue[queueused++] = i;
  }

  /* color */
  while (next < queueused) {
    for (i = 0; i < predused[queue[next]]; i++)
      if (a->f[preds[queue[next]][i]] != 1) {
	a->f[preds[queue[next]][i]] = 1;
	queue[queueused++] = preds[queue[next]][i];
      }
    next++;
  }
    
  for (i = 0; i < a->ns; i++) 
    mem_free(preds[i]);
  mem_free(preds);
  mem_free(predused);
  mem_free(predalloc);
  mem_free(queue);
}
