/* prefix.c */

/*
 * MONA is Copyright (C) 1997 BRICS. All rights reserved. 
 *
 * Reproduction of all or part of this software is permitted for
 * educational or research use on condition that this copyright notice is
 * included in any copy. This software comes with no warranty of any
 * kind. In no event will BRICS be liable for any damages resulting from
 * use of this software.
 */

#include <stdlib.h>
#include "dfa.h"

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
      preds[s] = (int *) mem_realloc(preds[s], sizeof(int) * predalloc[s]);
    }

    preds[s][predused[s]++] = current_state;
  }
  else {
    successors(bddm, ws1s___bdd_else(bddm, p));
    successors(bddm, ws1s___bdd_then(bddm, p));
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
    
  free(preds);
  free(predused);
  free(predalloc);
  free(queue);
}
