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
#include <string.h>
#include "dfa.h"
#include "../Mem/mem.h"

static int *bfs_queue;
static int *bfs_dist;
static int *bfs_prev;
static int bfs_current_distance; 
static unsigned bfs_current_state; 
static unsigned head, tail;

static void automaton_bfs_explore_leaf(unsigned leaf_value)
{ /* each leaf (except perhaps initial state) is visited exactly once */
  bfs_dist[leaf_value] = bfs_current_distance + 1;
  bfs_prev[leaf_value] = bfs_current_state;
  bfs_queue[head++] = leaf_value;
}

static void automaton_bfs(DFA *a, int *dist, int *prev)
{ 
  head = 1, tail = 0;
  bfs_dist = dist, bfs_prev = prev;
  bfs_queue = (int *) mem_alloc((a->ns+1)*sizeof(int));
  bfs_current_state = a->s;
  bfs_queue[0] = bfs_current_state;
  bfs_dist[bfs_current_state] = 0;  
  bfs_prev[bfs_current_state] = -1;
  bdd_prepare_apply1(a->bddm);

  while (tail < head) {
    bfs_current_state = bfs_queue[tail++];
    bfs_current_distance = bfs_dist[bfs_current_state];
    bdd_call_leafs(a->bddm, a->q[bfs_current_state], &automaton_bfs_explore_leaf);
  }
  mem_free(bfs_queue);
}

typedef struct intlist {
  int item;
  struct intlist *next;
} intlist;

char *dfaMakeExample(DFA *a, int polarity, int no_free_vars, unsigned *offsets)
{
  int i, j, min_dist = -1, minv, length;
  intlist *state_list, *ip;
  char *example;
  int *dist, *prev;

  dist = (int *) mem_alloc(a->ns * (sizeof(int))); /* distance from start */
  prev = (int *) mem_alloc(a->ns * (sizeof(int))); /* previous in path */

  automaton_bfs(a, dist, prev); /* breadth-first-search */
  /* dist[a->s]==0 iff initial state not reachable on path of length >0 */

  for (i = 0, minv = -1; i < a->ns; i++)
    if (a->f[i] == polarity)
      if ((minv == -1 || dist[i] < min_dist) && dist[i] >= 1) {
	minv = i;
	min_dist = dist[i];
      }

  if (min_dist == -1) {
    mem_free(dist);
    mem_free(prev);
    return NULL;
  }
  
  state_list = (intlist *) mem_alloc(sizeof(intlist));
  state_list->item = minv;
  state_list->next = NULL;

  for (length = 0; length < min_dist; length++) {
    minv = prev[minv];
    ip = (intlist *) mem_alloc(sizeof(intlist));
    ip->item = minv;
    ip->next = state_list;
    state_list = ip;      
  }
  
  example = (char *) mem_alloc((no_free_vars+1) * length * sizeof(char) + 1);
  for (i = 0; i < (no_free_vars+1) * length * sizeof(char); i++)
    example[i] = 1;
  example[(no_free_vars+1) * length] = 0;
  
  ip = state_list;
  j = 0;
  while (ip && ip->next) {
    trace_descr trace, tp;
    trace = find_one_path(a->bddm, a->q[ip->item], ip->next->item);
    
    for (i = 0; i < no_free_vars; i++) {
      tp = trace;
      while (tp && (tp->index != offsets[i])) 
	tp = tp->next;
      
      if (!tp)
	example[i*length+j] = 'X';
      else if (tp->value)
	example[i*length+j] = '1';
      else
	example[i*length+j] = '0';
    }
    
    kill_trace(trace);
    ip = ip->next;
    j++;
  }

  while (state_list) { 
    ip = state_list->next;   
    mem_free(state_list);        
    state_list = ip;           
  }           
  mem_free(state_list);
  mem_free(dist);
  mem_free(prev);

  return example;
}

static void print_example(char *example, char *name, 
			  int no_free_vars, char **free_variables, 
			  unsigned *offsets, char *types,
			  int treestyle)
{ 
  int i, j;
  int length;

  length = strlen(example)/(no_free_vars+1);

  if (treestyle) {
    printf("Free variables are: ");
    for (i = 0; i < no_free_vars; i++)
      printf ("%s%s", free_variables[i], i == no_free_vars-1 ? "" : ", ");
    
    printf("\n\nA %s of least length (%d) is:\n"
	   "Booleans:\n", name, length-1);
    for (i = 0; i < no_free_vars; i++)
      putchar(example[i*length]);
    
    printf("\nUniverse:\n");
    for (i = 1; i < length; i++) {
      putchar('(');
      for (j = 0; j < no_free_vars; j++)
	putchar(example[j*length+i]);
      putchar(',');
    }
    printf("()");
    for (i = 1; i < length; i++)
      printf(",())");
    printf("\n");
    
  } 
  else {
    printf("A %s of least length (%d) is:\n", 
	   name, length-1);
    for (i = 0; i < no_free_vars; i++) {
      printf("%-15s %c ", free_variables[i], example[i*length]);
      for (j = 0; j < length-1; j++)
	putchar(example[i*length+1+j]);
      printf("\n");
    }
    printf("\n");
    
    for (i = 0; i < no_free_vars; i++)
      switch (types[i])
	{
	case 0:
	  printf("%s = %s\n", free_variables[i],
		 example[i*length] == '1' ? "true" : "false");
	  break;
	case 1:
	  {
	    int j = 0;
	    while (example[i*length+j+1] == '0' && j < length) 
	      j++;
	    printf("%s = %i\n", free_variables[i], j);
	  }
	break;
	case 2:
	  {
	    int j, t = 0;
	    printf("%s = {", free_variables[i]);
	    for (j = 0; j < length; j++)
	      if (example[i*length+j+1] == '1') {
		if (t++ != 0)
		  printf(",");
		printf("%i",j);
	      }
	    printf("}\n");
	  }
	break;
	}
  }
  
  mem_free(example);
}

void dfaAnalyze(DFA *dfa, int no_free_vars, 
		char **free_variables, unsigned *offsets, char *types,
		int treestyle)
{
  char *counterexample, *satisfyingexample;

  counterexample = dfaMakeExample(dfa, -1, no_free_vars, offsets);
  satisfyingexample = dfaMakeExample(dfa, 1, no_free_vars, offsets);

  if (!counterexample && satisfyingexample)
    printf("Formula is valid\n");
  else if (!satisfyingexample)
    printf("Formula is unsatisfiable\n");
  
  if (counterexample) {
    if (!satisfyingexample)
      printf("\n");
    print_example(counterexample, "counter-example",
		  no_free_vars, free_variables, offsets, types, treestyle);
  }
  if (satisfyingexample){
    if (no_free_vars)
      printf("\n");
    print_example(satisfyingexample, "satisfying example",
		  no_free_vars, free_variables, offsets, types, treestyle);
  }
}

/* added -hr 7/01 */

int dfaIsExample(DFA *a, int polarity)
{
  int i, j, min_dist = -1, minv;
  int *dist, *prev;

  dist = (int *) mem_alloc(a->ns * (sizeof(int))); /* distance from start */
  prev = (int *) mem_alloc(a->ns * (sizeof(int))); /* previous in path */

  automaton_bfs(a, dist, prev); /* breadth-first-search */
  /* dist[a->s]==0 iff initial state not reachable on path of length >0 */

  for (i = 0, minv = -1; i < a->ns; i++)
    if (a->f[i] == polarity)
      if ((minv == -1 || dist[i] < min_dist) && dist[i] >= 1) {
	minv = i;
	min_dist = dist[i];
      }

 
  if (min_dist == -1) { 
    mem_free(dist);
    mem_free(prev);
    return 0;       /* there is no example */
  }
  else {
    mem_free(dist);
    mem_free(prev);
    return 1;         /* there exists an example */
  }
}

int dfaStatus(DFA *dfa)
{
  int isCounterexample;
  int isSatisfyingexample;

  isCounterexample = dfaIsExample(dfa, -1);
  isSatisfyingexample = dfaIsExample(dfa, 1);

  if (!isCounterexample && isSatisfyingexample)
    return 1;     /* Formula is valid */
  else if (!isSatisfyingexample)
    return -1;    /* Formula is unsatisfiable */
  else
    return 0;     /* Formula is satisfiable but not valid */
}
