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
#include "../Mem/mem.h"
#include "dyn.h"
#include "bdd_dump.h"

/* PairArray */

void paInit(PairArray *q) 
{ 
  q->m = 0; 
  q->allocated = q->used = 0; 
}

void paFree(PairArray *q) 
{
  mem_free(q->m);
}

void paInsert(PairArray *q, State i, State j) 
{ 
  if (q->used == q->allocated) { 
    q->allocated = q->used*2+2; 
    q->m = (State *) mem_resize(q->m, sizeof(State)*q->allocated*2); 
  } 
  q->m[q->used*2+0] = i; 
  q->m[q->used*2+1] = j; 
  q->used++; 
} 

/* BehaviourMatrix */

void initBM(BehaviourMatrix *b) 
{ 
  b->m = (bdd_handle *) mem_alloc(sizeof(bdd_handle)); 
  b->lf = b->rf = 0; 
  b->ls = b->rs = b->lu = b->ru = 1; 
}

void initBMtoSize(BehaviourMatrix *b, unsigned l, unsigned r) 
{ 
  b->m = (bdd_handle *) mem_alloc(sizeof(bdd_handle)*l*r); 
  b->lf = b->rf = 0; 
  b->ls = b->lu = l; 
  b->rs = b->ru = r; 
}

void extendLeftBM(BehaviourMatrix *b) 
{ 
  if (b->lu >= b->ls) { 
    unsigned l, r; 
    bdd_handle *t = 
      (bdd_handle *) mem_alloc(sizeof(bdd_handle)*(b->ls*2+1)*b->rs); 
    for (l = 0; l < b->lu; l++) 
      for (r = 0; r < b->ru; r++)
        t[l*b->rs+r] = b->m[l*b->rs+r]; 
    mem_free(b->m); 
    b->m = t; 
    b->ls = b->ls*2+1; 
  } 
  b->lu += 1; 
}

void extendRightBM(BehaviourMatrix *b) 
{
  if (b->ru >= b->rs) { 
    unsigned l, r; 
    bdd_handle *t = 
      (bdd_handle *) mem_alloc(sizeof(bdd_handle)*b->ls*(b->rs*2+1)); 
    for (l = 0; l < b->lu; l++) 
      for (r = 0; r < b->ru; r++) 
        t[l*(b->rs*2+1)+r] = b->m[l*b->rs+r]; 
    mem_free(b->m); 
    b->m = t; 
    b->rs = b->rs*2+1; 
  } 
  b->ru += 1; 
}

#ifndef NDEBUG
void dumpBM(BehaviourMatrix *bbb, bdd_manager *bddm)
{
  int i, j;
  printf("\nBEHAVIOUR:");
  for (i = 0; i < bbb->lf; i++) {
    for (j = 0; j < bbb->rf; j++)
      printf("%u ", BDD_ROOT(bddm, bbb->m[i*bbb->rs+j]));
    printf("\n");
  }
  bddDump(bddm);
}
#endif
