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
