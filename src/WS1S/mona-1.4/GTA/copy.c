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
#include "../Mem/mem.h"
#include "gta.h"

GTA *gtaCopy(GTA *P) 
{
  unsigned i, p1, p2;
  GTA *res = gtaMake();
  res->final = (int *) mem_alloc(sizeof(int)*P->ss[0].size);

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
