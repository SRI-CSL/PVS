/* dyn.h */

/*
 * MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
 *
 * Reproduction of all or part of this software is permitted for
 * educational or research use on condition that this copyright notice is
 * included in any copy. This software comes with no warranty of any
 * kind. In no event will BRICS be liable for any damages resulting from
 * use of this software.
 */

#ifndef __DYN_H
#define __DYN_H

#include "gta.h"

/* Dynamic array containing pairs of states */

typedef struct {
  State *m;
  unsigned allocated, used;
} PairArray;

#define paLeft(q, n) q.m[(n)*2+0]
#define paRight(q, n) q.m[(n)*2+1]

#define paSize(q) q.used

void paInit(PairArray *q); 
void paFree(PairArray *q); 
void paInsert(PairArray *q, State i, State j); 

/* Dynamic matrix containing bdd_handles */

typedef struct {
  bdd_handle *m;
  unsigned ls, rs; /* allocated size */
  unsigned lu, ru; /* used size */
  unsigned lf, rf; /* filled size */
} BehaviourMatrix;

#define BM(b, i, j) b.m[(i)*b.rs+(j)]

void initBM(BehaviourMatrix *b); 
void initBMtoSize(BehaviourMatrix *b, unsigned l, unsigned r);
void extendLeftBM(BehaviourMatrix *b);
void extendRightBM(BehaviourMatrix *b); 

#ifndef NDEBUG
void dumpBM(BehaviourMatrix *bbb, bdd_manager *bddm);
#endif

#endif
