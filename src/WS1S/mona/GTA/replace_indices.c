/* replace_indices.c */

/*
 * MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
 *
 * Reproduction of all or part of this software is permitted for
 * educational or research use on condition that this copyright notice is
 * included in any copy. This software comes with no warranty of any
 * kind. In no event will BRICS be liable for any damages resulting from
 * use of this software.
 */

#include "gta.h"

void gtaReplaceIndices(GTA *P, unsigned map[]) 
{
  unsigned i,p1,p2;
  
  for (i = 0; i < guide.numSs; i++) {
    unsigned rs = P->ss[guide.muRight[i]].size; 
    unsigned ls = P->ss[guide.muLeft[i]].size;

    bdd_prepare_apply1(P->ss[i].bddm);

    for (p1 = 0; p1 < ls; p1++) 
      for (p2 = 0; p2 < rs; p2++) 
	bdd_replace_indices(
	  P->ss[i].bddm, 
	  BDD_ROOT(P->ss[i].bddm, BEH(P->ss[i], p1, p2)),
	  map); 
  }
}
