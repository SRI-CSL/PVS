/* copy.c */

/*
 * MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
 *
 * Reproduction of all or part of this software is permitted for
 * educational or research use on condition that this copyright notice is
 * included in any copy. This software comes with no warranty of any
 * kind. In no event will BRICS be liable for any damages resulting from
 * use of this software.
 */

#include <stdlib.h>
#include "gta.h"

GTA *gtaCopy(GTA *P) 
{
  unsigned i, p1, p2;
  GTA *res = (GTA *) mem_alloc(sizeof(GTA));
  res->final = (int *) mem_alloc(sizeof(int)*P->ss[0].size);
  res->ss = (StateSpace *) mem_alloc(sizeof(StateSpace)*guide.numSs);

  /* copy finals */
  for (i = 0; i < P->ss[0].size; i++)
    res->final[i] = P->final[i];

  /* copy state spaces */
  for (i = 0; i < guide.numSs; i++) {
    StateSpace *ss = &res->ss[i];

    ss->initial = P->ss[i].initial;
    ss->size = P->ss[i].size;
    ss->ls = P->ss[i].ls;
    ss->rs = P->ss[i].rs;
    ss->behaviour = (bdd_handle *) mem_alloc(sizeof(bdd_handle)*ss->ls*ss->rs);
    ss->bddm = bdd_new_manager(ss->size*8, ((ss->size+3)/4)*4);
    
    bdd_prepare_apply1(P->ss[i].bddm);
    for (p1 = 0; p1 < P->ss[guide.muLeft[i]].size; p1++) {
      for (p2 = 0; p2 < P->ss[guide.muRight[i]].size; p2++) {
	bdd_apply1(P->ss[i].bddm, 
		   BDD_ROOT(P->ss[i].bddm, BEH(P->ss[i], p1, p2)), 
		   ss->bddm, 
		   &fn_identity);
	BEH((*ss), p1, p2) = BDD_LAST_HANDLE(ss->bddm);
      }
    }
  }
  
  return res;
}
