/* printdfa.c */

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

struct intlist_ {
  int item;
  struct intlist_ *next;
};

typedef struct intlist_ *intlist;

void kill_intlist(intlist ip)
{
  intlist ip_temp;

  while (ip) {
    ip_temp = ip;
    ip = ip->next;

    free(ip_temp);
  }
}

static intlist terminals;

unsigned dfa2graph_fn(unsigned value) 
{  
  intlist sip, sip_prev, new_sip;

  sip = terminals; sip_prev = NULL;
  while (sip) 
    if (sip->item < value) {
      sip_prev = sip;
      sip = sip->next;
    }
    else if (sip->item == value)
      return value;
    else
      break;
  
  new_sip = (intlist) mem_alloc(sizeof(struct intlist_));
  new_sip->item = value;
  new_sip->next = sip;

  if (sip_prev) 
    sip_prev->next = new_sip;
  else
    terminals = new_sip;

  return value;
}

void bfs(DFA *a, int *dist, int *prev)
{
  int i;
  bdd_manager *trash = bdd_new_manager(8*a->ns, ((a->ns+3)/4)*4);

  intlist *neighbors = (intlist *) mem_alloc(a->ns * (sizeof(intlist *)));
  boolean *visited = (boolean *) mem_alloc(a->ns * (sizeof(boolean)));
  int *queue = (int *) mem_alloc(a->ns*sizeof(int));
  int head = 1, tail = 0;
  queue[0] = a->s;

  for (i = 0; i < a->ns; i++) {
    dist[i] = 0;
    prev[i] = -1;
    visited[i] = FALSE;
    
    bdd_prepare_apply1(a->bddm);
    terminals = NULL;
    bdd_apply1(a->bddm, a->q[i], trash, &dfa2graph_fn);
    neighbors[i] = terminals;
  }
  
  while (tail < head) {
    int n = queue[tail++];
    intlist ip = neighbors[n];
    visited[n] = TRUE;
    while (ip) {
      if (!visited[ip->item]) {
	dist[ip->item] = dist[n] + 1;
	prev[ip->item] = n;
	queue[head++] = ip->item;
	visited[ip->item] = TRUE;
      }
      ip = ip->next;
    }
  }

  for (i = 0; i < a->ns; i++) 
    kill_intlist(neighbors[i]);

  bdd_kill_manager(trash);
  free(neighbors);
  free(visited);  
  free(queue);
}

char *dfaMakeExample(DFA *a, int polarity, 
		     int no_free_vars, unsigned *offsets)
{
  int i, j, min_dist = -1, minv, length;
  intlist state_list, ip;
  char *example;
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

  if (min_dist == -1) {
    free(dist);
    free(prev);
    return NULL;
  }
  
  state_list = (intlist) mem_alloc(sizeof(struct intlist_));
  state_list->item = minv;
  state_list->next = NULL;
  
  length = 0;
  while (length < min_dist) {
    minv = prev[minv];
    ip = (intlist) mem_alloc(sizeof(struct intlist_));
    ip->item = minv;
    ip->next = state_list;
    
    state_list = ip;      
    length++;
  }
  
  example = (char *) mem_alloc ((no_free_vars+1) * length * sizeof(char) + 1);
  for (i = 0; i < (no_free_vars+1) * length * sizeof(char); i++)
    example[i] = 1;
  example[(no_free_vars+1) * length * sizeof(char)] = 0;
  
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
    free(state_list);        
    state_list = ip;           
  }           
  free(state_list);
  free(dist);
  free(prev);

  return example;
}

void print_example(char *example, char *name, char *desc,
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
    
    printf("\n\nA %s (for %s) of least length (%d) is:\n"
	   "Booleans:\n", name, desc, length-1);
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
    printf("A %s (for %s) of least length (%d) is:\n", 
	   name, desc, length-1);
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
  
  free(example);
}

void dfaAnalyze(DFA *impl, DFA *conj, int no_free_vars, 
		char **free_variables, unsigned *offsets, char *types,
		int treestyle)
{
  char *counterexample, *satisfyingexample;

  counterexample = dfaMakeExample(impl, -1, no_free_vars, offsets);
  satisfyingexample = dfaMakeExample(conj, 1, no_free_vars, offsets);

  if (!counterexample)
    printf("Formula is valid\n");
  else if (!satisfyingexample)
    printf("Formula is unsatisfiable\n");
  
  if (counterexample) {
    if (!satisfyingexample)
      printf("\n");
    print_example(counterexample, 
		  "counter-example", "assertion => main",
		  no_free_vars, free_variables, offsets, types, treestyle);
  }
  if (satisfyingexample){
    if (no_free_vars)
      printf("\n");
    print_example(satisfyingexample, 
		  "satisfying example", "assertion & main",
		  no_free_vars, free_variables, offsets, types, treestyle);
  }
}
