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
#include "gta.h"

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
  if (inherited_acceptance)
    gtaFreeInheritedAcceptance(inheritedAcceptance);
}

void gtaPrintVitals(GTA* a)
{ 
  unsigned d;
  unsigned total_statespace_size = 0;
  unsigned total_BDD_size = 0;

  for (d = 0; d < guide.numSs; d++) {
    printf("State space %d '%s': %d state%s, %d BDD node%s\n",
	   d, guide.ssName[d], 
	   a->ss[d].size, a->ss[d].size>1?"s":"", 
	   mona_bdd_size(a->ss[d].bddm), mona_bdd_size(a->ss[d].bddm)>1?"s":"");
    total_statespace_size += a->ss[d].size;
    total_BDD_size += mona_bdd_size(a->ss[d].bddm);
  }
  printf("Total: %d state%s, %d BDD node%s\n", 
	 total_statespace_size, total_statespace_size>1?"s":"", 
	 total_BDD_size, total_BDD_size>1?"s":"");
}

void gtaPrintTotalSize(GTA *a)
{
  unsigned d;
  unsigned total_statespace_size = 0;
  unsigned total_BDD_size = 0;

  for (d = 0; d < guide.numSs; d++) {
    total_statespace_size += a->ss[d].size;
    total_BDD_size += mona_bdd_size(a->ss[d].bddm);
  }
  printf("\nAutomaton has %d state%s and %d BDD node%s\n", 
	 total_statespace_size, total_statespace_size>1?"s":"", 
	 total_BDD_size, total_BDD_size>1?"s":"");
}
