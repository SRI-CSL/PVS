/* printgta.c */

/*
 * MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
 *
 * Reproduction of all or part of this software is permitted for
 * educational or research use on condition that this copyright notice is
 * included in any copy. This software comes with no warranty of any
 * kind. In no event will BRICS be liable for any damages resulting from
 * use of this software.
 */

#include <stdio.h>
#include "gta.h"

int gta_table_size;

void increment_gta_table_size(bdd_record *node_pointer)
{
  unsigned indx, l, r;
  LOAD_lri(node_pointer, l, r, indx); 
  if (indx == BDD_LEAF_INDEX) 
    gta_table_size++;
}

void gta_transition_table_size(GTA* P)
{
  unsigned i, l, r;
  gta_table_size = 0;
  for (i = 0; i < guide.numSs; i++) {
    /* remove all markings in P->ss[i].bddm */
    bdd_prepare_apply1(P->ss[i].bddm);
    for (l = 0; l < P->ss[guide.muLeft[i]].size; l++) {
      for ( r = 0; r < P->ss[guide.muRight[i]].size; r++)
    /* count number of states reachable from (P->ss[i].behaviour[l*rs+r]*/
    bdd_operate_on_nodes(P->ss[i].bddm,
			 BDD_ROOT(P->ss[i].bddm, BEH(P->ss[i], l, r)), 
			 increment_gta_table_size); 
    }
  }
}

void gtaPrintVerbose(GTA* P)
{
  unsigned lp,rp,p,a = 0;

  printf("Resulting GTA:\n"
	 "Accepting states: ");
  for (p = 0; p < P->ss[0].size; p++)
    if (P->final[p] == 1) printf("%d ", p);
  printf("\nRejecting states: ");
  for (p = 0; p < P->ss[0].size; p++)
    if (P->final[p] == -1) printf("%d ", p);
  printf("\nDon't-care states: ");
  for (p = 0; p < P->ss[0].size; p++)
    if (P->final[p] == 0) {
      a = 1;
      break;
    }
  if (a) {
    for (p = 0; p < P->ss[0].size; p++)
      if (P->final[p] == 0) printf("%d ", p);
    printf("\n");
  }
  
  for(p = 0; p < guide.numSs; p++) {
    printf("\nState space %d '%s' (size %d):\n", 
	   p, guide.ssName[p], P->ss[p].size);
    printf("Initial state: %d\n", P->ss[p].initial);
    printf("Transitions:\n");

#ifdef DUMP_RAW
    for(lp = 0; lp < P->ss[guide.muLeft[p]].size; lp++) 
      for(rp = 0; rp < P->ss[guide.muRight[p]].size; rp++)
	printf("(%d,%d) node: %d\n", 
	       lp, rp, BDD_ROOT(P->ss[p].bddm, BEH(P->ss[p], lp, rp)));
    printf("BDD nodes:\n");
    bddDump(P->ss[p].bddm);
#else
    for(lp = 0; lp < P->ss[guide.muLeft[p]].size; lp++) 
      for(rp = 0; rp < P->ss[guide.muRight[p]].size; rp++) 
	print_bddpaths_verbose(lp, rp, P->ss[p].bddm,
			       BDD_ROOT(P->ss[p].bddm, BEH(P->ss[p], lp, rp)));
#endif
  }
  printf("\n"); 
}


void gtaPrint(GTA* P, unsigned *offs, unsigned no_offs, char **free_vars,
	      int inherited_acceptance)
{
  unsigned lp, rp, p, any = 0;
  boolean ***inheritedAcceptance = 0;

  if (inherited_acceptance)
    inheritedAcceptance = gtaCalcInheritedAcceptance(P);
  
  printf("GTA for formula with free variables: ");
  for (p = 0; p < no_offs; p++)
    printf ("%s ", free_vars[p]);

  printf("\nAccepting states: ");
  for (p = 0; p < P->ss[0].size; p++)
    if (P->final[p] == 1) printf("%d ", p);
  printf("\nRejecting states: ");
  for (p = 0; p < P->ss[0].size; p++)
    if (P->final[p] == -1) printf("%d ", p);
  for (p = 0; p < P->ss[0].size; p++)
    if (P->final[p] == 0) {
      any = 1;
      break;
    }
  if (any) {
    printf("\nDon't-care states: ");
    for (p = 0; p < P->ss[0].size; p++)
      if (P->final[p] == 0) printf("%d ", p);
  }
  printf("\n");
  
  for (p = 0; p < guide.numSs; p++) {
    printf("\nState space %d '%s' (size %d):\n",p, guide.ssName[p], P->ss[p].size);
    printf("Initial state: %d\n", P->ss[p].initial);
    printf("Transitions:\n");
    for (lp = 0; lp < P->ss[guide.muLeft[p]].size; lp++) {
      for (rp = 0; rp < P->ss[guide.muRight[p]].size; rp++) 
        print_bddpaths(lp, rp, P->ss[p].bddm,
		       BDD_ROOT(P->ss[p].bddm, BEH(P->ss[p], lp, rp)), 
		       no_offs, 
		       offs);
    }
    if (inherited_acceptance) {
      int s, k;
      printf("Inherited-acceptance:\n");
      for (k = 1; k <= 7; k++) {
	for (s = 0; s < P->ss[p].size; s++)
	  if (inheritedAcceptance[p][s][-1] +
	      inheritedAcceptance[p][s][0]*2 +
	      inheritedAcceptance[p][s][1]*4 == k)
	    break;
	if (s < P->ss[p].size) {
	  char *kind[] = {
	    "reject", "don't care", "don't care or reject", 
	    "accept", "accept or reject", "accept or don't care",
	    "anything"
	  };
	  printf("States leading to %s: ", kind[k-1]);
	  for (s = 0; s < P->ss[p].size; s++)
	    if (inheritedAcceptance[p][s][-1] +
		inheritedAcceptance[p][s][0]*2 +
		inheritedAcceptance[p][s][1]*4 == k)
	      printf("%d ", s);
	  printf("\n");
	}
      }
    }
  }
  gta_transition_table_size(P);
  printf("\nNumber of transitions: %i\n", gta_table_size);  
  if (inherited_acceptance)
    gtaFreeInheritedAcceptance(inheritedAcceptance);
}

void gtaPrintVitals(GTA* P)
{ 
  unsigned d;
  unsigned total_statespace_size = 0;
  unsigned total_BDD_size = 0;

  printf("Conjunctive automaton:\n");
  for (d = 0; d < guide.numSs; d++) {
    printf("State space %d '%s': %d state%s, %d BDD node%s\n",
	   d, guide.ssName[d], 
	   P->ss[d].size, P->ss[d].size>1?"s":"", 
	   bdd_size(P->ss[d].bddm), bdd_size(P->ss[d].bddm)>1?"s":"");
    total_statespace_size += P->ss[d].size;
    total_BDD_size += bdd_size(P->ss[d].bddm);
  }
  printf("Total: %d state%s, %d BDD node%s\n", 
	 total_statespace_size, total_statespace_size>1?"s":"", 
	 total_BDD_size, total_BDD_size>1?"s":"");
}
