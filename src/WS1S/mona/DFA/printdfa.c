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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "dfa.h"

unsigned table_size;

void increment_table_size(bdd_record *node_pointer)
{
  unsigned indx, l, r;
  LOAD_lri(node_pointer, l, r, indx); 
  if (indx == BDD_LEAF_INDEX) 
    table_size++;
}

unsigned transition_table_size(DFA *a)
{
  int i;
  table_size = 0;
  for (i = 0; i < a->ns; i++) {
    /* remove all markings in a->bddm */
    bdd_prepare_apply1(a->bddm); 
    /* count number of states reachable from a->q[i] */
    bdd_operate_on_nodes(a->bddm, a->q[i], increment_table_size); 
  }
  return table_size;
}

void dfaPrintVitals(DFA *a)
{
  unsigned i = ws1s___bdd_size(a->bddm);
  unsigned t = transition_table_size(a);
  
  printf("\nConjunctive automaton has %d state%s, %d BDD-node%s and %d transition%s\n", 
	 a->ns, a->ns==1 ? "" : "s", 
	 i, i==1 ? "" : "s",
	 t, t==1 ? "" : "s");
}

void dfaPrint(DFA *a, int no_free_vars, char **free_variables, unsigned *offsets)
{
  paths state_paths, pp;
  trace_descr tp;
  int i, j, any = 0;

  printf("DFA for formula with free variables: ");

  for (i = 0; i < no_free_vars; i++)
    printf ("%s ", free_variables[i]);

  printf("\nInitial state: %d\n", a->s);
  printf("Accepting states: ");
  for (i = 0; i < a->ns; i++)
    if (a->f[i] == 1)
      printf ("%d ", i);

  printf("\n");

  printf("Rejecting states: ");
  for (i = 0; i < a->ns; i++)
    if (a->f[i] == -1)
      printf ("%d ", i);

  printf("\n");
 
  for (i = 0; i < a->ns; i++)
    if (a->f[i] == 0) {
      any = 1;
      break;
    }
  if (any) {
    printf("Don't-care states: ");
    for (i = 0; i < a->ns; i++)
      if (a->f[i] == 0)
	printf ("%d ", i);

    printf("\n");
  }


  dfaPrintVitals(a);

  printf ("Transitions:\n");

  for (i = 0; i < a->ns; i++) {
    state_paths = pp = make_paths(a->bddm, a->q[i]);

    while (pp) {
      printf ("State %d: ", i);

      for (j = 0; j < no_free_vars; j++) {
	for (tp = pp->trace; tp && (tp->index != offsets[j]); tp = tp->next);

	if (tp) {
	  if (tp->value) printf("1");
	  else printf("0");
	}
	else
	  printf("X");	    
      }

      printf (" -> state %d\n", pp->to);
	
      pp = pp->next;
    }

    kill_paths(state_paths);
  }
}

/*
void printdfa_vcg(DFA *a, int no_free_vars, char **free_variables, unsigned *offsets)
{
  paths state_paths, pp;
  trace_descr tp;
  int i, j, k, l;
  char **buffer;
  int *used, *allocated;
  
  printf("graph: {\n"
	 " display_edge_labels: yes\n"
	 " layoutalgorithm: dfs\n"
	 " finetuning: yes\n"
	 " near_edges: yes\n"
	 " orientation: left_to_right\n"
	 " splines: yes\n"
	 " node.shape: ellipse\n"
	 " node.width: 50\n"
	 " node.height: 50\n"
	 " node.textmode: center\n"
	 " node: {\n"
	 "  title: \"dummy\"\n"
	 "  label: \"\"\n"
	 "  borderwidth: 0\n"
	 " }\n");

  for (i = 0; i < a->ns; i++)
    printf(" node: {\n"
	   "  title: \"%d\"\n"
	   "  label: \"%c\"\n"
	   " }\n", i, (a->f[i] == -1) ? '-' : ((a->f[i] == 1) ? '+' : '0'));

  printf(" edge: {\n"
	 "  sourcename: \"dummy\"\n"
	 "  targetname: \"%d\"\n"
	 " }\n", a->s);

  buffer = (char **) mem_alloc(sizeof(char *) * a->ns);
  used = (int *) mem_alloc(sizeof(int) * a->ns);
  allocated = (int *) mem_alloc(sizeof(int) * a->ns);

  for (i = 0; i < a->ns; i++) {
    state_paths = pp = make_paths(a->bddm, a->q[i]);
    
    for (j = 0; j < a->ns; j++) {
      buffer[j] = 0;
      used[j] = allocated[j] = 0;
    }

    while (pp) {
      if (used[pp->to] >= allocated[pp->to]) {
	allocated[pp->to] = allocated[pp->to]*2+2;
	buffer[pp->to] = 
	  (char *) mem_realloc(buffer[pp->to], sizeof(char) * 
			       allocated[pp->to] * no_free_vars);
      }

      for (j = 0; j < no_free_vars; j++) {
	char c;
	for (tp = pp->trace; tp && (tp->index != offsets[j]); tp = tp->next);

	if (tp) {
	  if (tp->value) 
	    c = '1';
	  else 
	    c = '0';
	}
	else
	  c = 'X';

	buffer[pp->to][no_free_vars*used[pp->to] + j] = c;
      }
      used[pp->to]++;
      pp = pp->next;
    }

    for (j = 0; j < a->ns; j++) 
      if (buffer[j]) {
	printf(" edge: {\n"
	       "  sourcename: \"%d\"\n"
	       "  targetname: \"%d\"\n"
	       "  label: \"", i, j);
	for (k = 0; k < no_free_vars; k++) {
	  for (l = 0; l < used[j]; l++) {
	    putchar(buffer[j][no_free_vars*l+k]);
	    if (l+1 < used[j]) {
	      if (k+1 == no_free_vars)
		putchar(',');
	      else
		putchar(' ');
	    }
	  }
	  if (k+1 < no_free_vars)
	  printf("\\n");
	}
	printf ("\"\n"
		" }\n");
	free(buffer[j]);
      }

    kill_paths(state_paths);
  }

  free(allocated);
  free(used);
  free(buffer);

  printf("}\n");
}
*/

void dfaPrintGraphviz(DFA *a, int no_free_vars, unsigned *offsets)
{
  paths state_paths, pp;
  trace_descr tp;
  int i, j, k, l;
  char **buffer;
  int *used, *allocated;
  
  printf("digraph MONA_DFA {\n"
	 " rankdir = LR;\n"
	 " center = true;\n"
	 " size = \"7.5,10.5\";\n"
	 " edge [fontname = Courier];\n"
	 " node [height = .5, width = .5];\n"
	 " node [shape = doublecircle];");
  for (i = 0; i < a->ns; i++)
    if (a->f[i] == 1)
      printf(" %d", i);
  printf(";\n"
	 " node [shape = circle];");
  for (i = 0; i < a->ns; i++)
    if (a->f[i] != 1)
      printf(" %d", i);
  printf(";\n"
	 " init [shape = plaintext, label = \"\"];\n"
	 " init -> %d;\n", a->s);

  buffer = (char **) mem_alloc(sizeof(char *) * a->ns);
  used = (int *) mem_alloc(sizeof(int) * a->ns);
  allocated = (int *) mem_alloc(sizeof(int) * a->ns);

  for (i = 0; i < a->ns; i++) {
    state_paths = pp = make_paths(a->bddm, a->q[i]);
    
    for (j = 0; j < a->ns; j++) {
      buffer[j] = 0;
      used[j] = allocated[j] = 0;
    }

    while (pp) {
      if (used[pp->to] >= allocated[pp->to]) {
	allocated[pp->to] = allocated[pp->to]*2+2;
	buffer[pp->to] = 
	  (char *) mem_realloc(buffer[pp->to], sizeof(char) * 
			       allocated[pp->to] * no_free_vars);
      }

      for (j = 0; j < no_free_vars; j++) {
	char c;
	for (tp = pp->trace; tp && (tp->index != offsets[j]); tp = tp->next);

	if (tp) {
	  if (tp->value) 
	    c = '1';
	  else 
	    c = '0';
	}
	else
	  c = 'X';

	buffer[pp->to][no_free_vars*used[pp->to] + j] = c;
      }
      used[pp->to]++;
      pp = pp->next;
    }

    for (j = 0; j < a->ns; j++) 
      if (buffer[j]) {
	printf(" %d -> %d [label=\"", i, j);
	for (k = 0; k < no_free_vars; k++) {
	  for (l = 0; l < used[j]; l++) {
	    putchar(buffer[j][no_free_vars*l+k]);
	    if (l+1 < used[j]) {
	      if (k+1 == no_free_vars)
		putchar(',');
	      else
		putchar(' ');
	    }
	  }
	  if (k+1 < no_free_vars)
	  printf("\\n");
	}
	printf ("\"];\n");
	free(buffer[j]);
      }

    kill_paths(state_paths);
  }

  free(allocated);
  free(used);
  free(buffer);

  printf("}\n");
}

void dfaPrintVerbose(DFA *a)
{
  paths state_paths, pp;
  trace_descr tp;
  int i;

  printf ("Resulting DFA:\n");

  printf("Initial state: %d\n", a->s);
  printf("Accepting states: ");  
  for (i = 0; i < a->ns; i++)
    if (a->f[i] == 1)
      printf ("%d ", i);

  printf("\n");

  printf("Rejecting states: ");
  for (i = 0; i < a->ns; i++)
    if (a->f[i] == -1)
      printf ("%d ", i);

  printf("\n");
 
  printf("Don't-care states: ");
  for (i = 0; i < a->ns; i++)
    if (a->f[i] == 0)
      printf ("%d ", i);

  printf("\n");

  printf ("Transitions:\n");

  for (i = 0; i < a->ns; i++) {
    state_paths = pp = make_paths(a->bddm, a->q[i]);

    while (pp) {
      printf ("State %d: ", i);

      for (tp = pp->trace; tp; tp = tp->next) {
	printf("#%d=%c", tp->index, tp->value ? '1' : '0');
	if (tp->next)
	  printf(", ");
      }
      
      printf (" -> state %d\n", pp->to);
      pp = pp->next;
    }

    kill_paths(state_paths);
  }
  printf("\n");
}
