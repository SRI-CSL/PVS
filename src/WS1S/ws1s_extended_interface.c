/*
 * Extracted from Mona Code... (hr 11/98)
 */

#include <stdlib.h>
#include "./mona-1.3/DFA/dfa.h"

int dfaCheck(DFA *a, int polarity)
{
  int i, j, min_dist = -1, minv;
  int *dist, *prev;

  dist = (int *) mem_alloc(a->ns * (sizeof(int))); /* distance from start */
  prev = (int *) mem_alloc(a->ns * (sizeof(int))); /* previous in path */

  bfs(a, dist, prev); /* breadth-first-search */

  for (i = 0, minv = -1; i < a->ns; i++)
    if (a->f[i] == polarity)
      if ((minv == -1 || dist[i] < min_dist) && dist[i] >= 1) {
	minv = i;
	min_dist = dist[i];
      }

   free(dist);
   free(prev);

   return (min_dist == -1)? 0 : 1;
}

int dfaIsFull(DFA *a)
{
  return dfaCheck(a, -1) != 0;   /* no counterexample */
}

int dfaIsEmpty(DFA *a)
{
  return dfaCheck(a, 1) != 0;   /* no witness */
}
