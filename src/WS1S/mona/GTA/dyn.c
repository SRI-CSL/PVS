/* dyn.c */

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
#include <stdlib.h>
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
  free(q->m);
}

void paInsert(PairArray *q, State i, State j) 
{ 
  if (q->used == q->allocated) { 
    q->allocated = q->used*2+2; 
    q->m = (State *) mem_realloc(q->m, sizeof(State)*q->allocated*2); 
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
    free(b->m); 
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
    free(b->m); 
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
