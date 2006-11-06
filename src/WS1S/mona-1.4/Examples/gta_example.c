/* gta_example.c - GTA package example dummy application */

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
#include <stdio.h>
#include <string.h> 
#include "gta.h"

Guide guide;

char *my_global_variable;
bdd_manager *global_bddm;

void check_off_index(bdd_record *node_pointer)
     /*this function is written using primitives at a lower level
       than using the mona_bdd_then and mona_bdd_else; the reason is that 
       bdd_operate_on_nodes requires a C pointer a BDD node, not
       something of the type bdd_ptr */
{
  bdd_ptr low, high;
  unsigned index;
  LOAD_lri(node_pointer, low, high, index);
  if (index != BDD_LEAF_INDEX) {
    if (my_global_variable[index] == 0)
      my_global_variable[index] = 1;
  } 
  else {}
}

int main () {
  GTA* G;
  char **free_vars;
  int *orders; 
  unsigned numvars;
  SSSet *statespaces;

  /*read the automaton from a file, the TRUE argument means that the
    guide is being defined by the automaton description in the file */
  G = gtaImport("html.gta", &free_vars, &orders, &statespaces, TRUE);
  if(!G) {
    printf("error: file 'html.gta' not found (run 'mona -n html.mona')\n");
    exit(1);
  }
  printf("Guide: number of state spaces is %d\n", guide.numSs);
  
  /* illustrate use of state space names */
  {
    SsId d;    
    for (d = 0; d < guide.numSs; d++) 
      printf("State space %s is numbered %d\n", guide.ssName[d], d);
  } 
  
  /* illustrate use of inherited acceptance information */
  {
    SsId d;
    State p;
    int s;
    boolean ***inheritedAcceptance = gtaCalcInheritedAcceptance(G); 
    for (d = 0; d < guide.numSs; d++) {
      printf("State space %d\n", d);
      for (p = 0; p < G->ss[d].size; p++)
        for (s = 1; s >= -1; s--)
          printf(" State %d can%s lead to %s\n", 
                 p, inheritedAcceptance[d][p][s]? "":  " not",
                 (s==1)? "accept":(s==0)? "don't care": "reject");
    }
    gtaFreeInheritedAcceptance(inheritedAcceptance);
  }
  
  /* illustrate how to find indices of free variables and how to find
     out about their order (Boolean, first-order, or second-order) */
  {
    unsigned i;
    numvars = 0;
    while (free_vars[numvars] != 0)
      numvars++;
    printf("Number of free variables: %d\n", numvars);
    if (numvars != 6) {                                      
      printf("Oops\n");
      exit(1);
    } 
    /* get table index of variable G1 */
    for (i = 0; i < numvars; i++)
      if (strcmp(free_vars[i], "G1")==0)
        break;
    if (i == numvars) 
      printf("G1 not found\n");
    else {
      printf("G1 has index %d and is %d. order\n", i, orders[i]);
    }
  }
  
  /* illustrate how to analyze the BDD associated with an entry in the
     transition table */
  
  { /* lookup, say, transition corresponding to state space 1,
       state pair (left, right) = (4,3) */
    unsigned i;
    bdd_ptr my_bdd_ptr = BDD_ROOT(G->ss[1].bddm, BEH(G->ss[1], 4, 3));
    char var_is_used[numvars]; /* var_is_used[i] is true iff variable i
                                  is the index of some BDD node in
                                  the DAG rooted in my_bdd_ptr */
    printf("State space 0 goes to (%d, %d)\n", 
           guide.muLeft[0], guide.muRight[0]);
    printf("State space 1 goes to (%d, %d)\n", 
           guide.muLeft[1], guide.muRight[1]);
    
    printf("Number of BDD nodes in state space 1: %d\n", mona_bdd_size(G->ss[1].bddm));
    for (i = 0; i < numvars; i++)
      var_is_used[i] = 0;
    bdd_prepare_apply1(G->ss[1].bddm);
    my_global_variable = var_is_used;
    global_bddm = (G->ss[1].bddm);
    bdd_operate_on_nodes(G->ss[1].bddm, my_bdd_ptr, &check_off_index);
    
    for (i = 0; i < numvars; i++)
      if (var_is_used[i])
        printf("Variable %s in use for transitions from (4,3) in state space 1\n",
               free_vars[i]); 
  }
  
  /* illustrate how to perform a lookup in the transition table */
  {
    char bit_vector[6] = {0, 1, 1, 0, 1, 0};
    bdd_manager *bddm = G->ss[1].bddm;
    bdd_ptr my_bdd_ptr = BDD_ROOT(bddm, BEH(G->ss[1], 0, 2));
    while (!bdd_is_leaf(bddm, my_bdd_ptr)) {
      my_bdd_ptr =  (bit_vector[bdd_ifindex(bddm, my_bdd_ptr)])?
        mona_bdd_then(bddm, my_bdd_ptr):
        mona_bdd_else(bddm, my_bdd_ptr);
    }
    printf("State reached from (0,2) on {0, 1, 1, 0, 1, 0} is %d\n", 
           bdd_leaf_value(bddm, my_bdd_ptr));
  }

  return 0;
}
