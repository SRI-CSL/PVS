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

#include <stdio.h>
#include <stdlib.h>
#include "dfa.h"
#include "bdd_external.h"
#include "../Mem/mem.h"

/* EXPORT */

int dfaExport(DFA *a, char *filename, int num, char *vars[], char orders[])
{
  Table *table = tableInit(); 
  int i;
  unsigned int j;
  FILE *file;

  if (filename) {
    if ((file = fopen(filename, "w")) == 0) 
      return 0;
  }
  else
    file = stdout;
  
  /* remove all marks in a->bddm */
  bdd_prepare_apply1(a->bddm); 
  
  /* build table of tuples (idx,lo,hi) */
  for (i = 0; i < a->ns; i++)  
    export(a->bddm, a->q[i], table);

  /* renumber lo/hi pointers to new table ordering */
  for (j = 0U; j < table->noelems; j++) {
    if (table->elms[j].idx != -1) {
      table->elms[j].lo = bdd_mark(a->bddm, table->elms[j].lo) - 1;
      table->elms[j].hi = bdd_mark(a->bddm, table->elms[j].hi) - 1;
    }
  }

  /* write to file */
  fprintf(file,
          "MONA DFA\n"
	  "number of variables: %u\n"
	  "variables:", num);
  for (i = 0; i < num; i++)
    fprintf(file, " %s", vars[i]);
  fprintf(file, "\n"
	  "orders:");
  for (i = 0; i < num; i++)
    fprintf(file, " %u", (unsigned) orders[i]);
  fprintf(file, "\n"
          "states: %u\n"
          "initial: %u\n"
          "bdd nodes: %u\n"
          "final:",
	  a->ns, a->s, table->noelems); 
  for (i = 0; i < a->ns; i++)
    fprintf(file, " %d", a->f[i]);
  fprintf(file, "\nbehaviour:");
  for (i = 0; i < a->ns; i++)
    fprintf(file, " %u", bdd_mark(a->bddm, a->q[i]) - 1);
  fprintf(file, "\nbdd:\n");
  for (j = 0U; j < table->noelems; j++) 
    fprintf(file, " %i %u %u\n", 
	    table->elms[j].idx, table->elms[j].lo, table->elms[j].hi);
  fprintf(file, "end\n");

  tableFree(table);
  if (filename)
    fclose(file);
  return 1;
}

/* IMPORT */

extern BddNode *table;
extern bdd_manager *import_bddm;

DFA *dfaImport(char* filename, char ***vars, int **orders)
{
  int i, numvars, bdd_nodes, ns, s;
  FILE *file;
  DFA *a;
  char ts[100];
  int ti;

  fprintf(stderr, "IN dfaImport\n");
  fflush(stderr);
  
  /* Read file */
  if ((file = fopen(filename, "r")) == 0) 
    return NULL;

  /*
   * BD: we should check the return value of every call to fscanf.
   */
  if (fscanf(file,
	     "MONA DFA\n"
	     "number of variables: %u\n"
	     "variables: ", &numvars) != 1) {
    return NULL;
  }

  /*
   * BD: allocate vars and order arrays here if needed.
   * Also initialize a and table to NULL in case we need to clean up.
   */
  a = NULL;
  table = NULL;
  if (vars) {
    *vars = (char **) mem_alloc(sizeof(char *) * (numvars + 1));
    // BD: initialize all to NULL so that we can release memory
    // in case of a read or format error.
    //    (*vars)[numvars] = 0;
    for (i=0; i<= numvars; i++) {
      (*vars)[i] = NULL;
    }
  }
  if (orders) {
    *orders = (int *) mem_alloc(sizeof(int) * numvars);
  }


  if (vars) {
    for (i = 0; i < numvars; i++) {
      (*vars)[i] = (char *) mem_alloc(100);
      if (fscanf(file, " %s ", (*vars)[i]) != 1) {
	goto cleanup;
      }
    }
  }
  else {
    for (i = 0; i < numvars; i++) {
      if (fscanf(file, " %s ", ts) != 1) {
	goto cleanup;
      }
    }
  }
  if (fscanf(file, "orders: ") != 0) {
    goto cleanup;
  }
  if (orders) {
    for (i = 0; i < numvars; i++)
      if (fscanf(file, " %d ", &((*orders)[i])) != 1) {
	goto cleanup;
      }
  }
  else {
    for (i = 0; i < numvars; i++) {
      if (fscanf(file, " %d ", &ti) != 1) {
	goto cleanup;
      }
    }
  }

  if (fscanf(file,
             "states: %u\n"
             "initial: %u\n"
             "bdd nodes: %u\n"
             "final:", 
             &ns, &s, &bdd_nodes) != 3) {
    goto cleanup;
  }

  a = dfaMake(ns);
  a->s = s;

  for (i = 0; i<a->ns; i++) {
    if (fscanf(file, " %d", &a->f[i]) != 1) {
      goto cleanup;
    }
  }

  if (fscanf(file, "\nbehaviour:") != 0) {
    goto cleanup;
  }
  for (i = 0; i < a->ns; i++) {
    if (fscanf(file, " %u", &a->q[i]) != 1) {
      goto cleanup;
    }
  }

  if (fscanf(file, "\nbdd:\n") != 0) {
    goto cleanup;
  }
  table = (BddNode *) mem_alloc(sizeof(BddNode)*bdd_nodes);
  
  for (i = 0; i < bdd_nodes; i++) {
    table[i].p = -1;
    if (fscanf(file, "%i %u %u\n", 
	       &table[i].idx,
	       &table[i].lo,
	       &table[i].hi) != 3) {
      goto cleanup;
    }
  }
  
  if (fgetc(file) != 'e' || fgetc(file) != 'n' || fgetc(file) != 'd') {
    goto cleanup;
  }

  fclose(file);

  /* fill bdd-manager */
  import_bddm = a->bddm;
  for (i = 0; i < a->ns; i++) 
    a->q[i] = make_node(a->q[i]);

  mem_free(table);
  return a;

 cleanup:
  fclose(file);
  if (table != NULL) {
    mem_free(table);
  }
  if (a != NULL) {
    dfaFree(a);
  }
  if (orders) {
    mem_free(*orders);
  }
  if (vars) {
    i = 0;
    while ((*vars)[i] != NULL) {
      mem_free((*vars)[i]);
      i ++;
    }
    mem_free(*vars);
  }

  return NULL;
}
