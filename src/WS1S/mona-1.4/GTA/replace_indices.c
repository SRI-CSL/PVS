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
