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
#include "dyn.h" /* PairArray, BehaviourMatrix */
#include "pairhash.h" /* PairHashTable */

#define STATUS_TO_BOOL(s) ((s == -1) ? 0 : 1)
#define BOOL_TO_STATUS(s) ((s == 0) ? -1 : 1)

BehaviourMatrix *b; /* extendable matrices */ 
PairArray *pairs; /* map states to pairs of states */
PairHashTable *inverse; /* inverses for 'pairs' */
GTA *aa1, *aa2, *res; /* automata */
SsId s, lSs, rSs; /* current, left and right state spaces */

static unsigned fn_pair(unsigned p, unsigned q)
{
  unsigned n;

  /* is the pair new for this state space? */
  if (!lookupPHT(&inverse[s], p, q, &n)) {
    SsId i;

    /* make new state for the pair */
    n = paSize(pairs[s]);
    insertPHT(&inverse[s], p, q, n);
    paInsert(&pairs[s], p, q);

    /* extend matrices */
    for (i = 0; i < guide.numHitsLeft[s]; i++)
      extendLeftBM(&b[guide.hitsLeft[s][i]]);
    for (i = 0; i < guide.numHitsRight[s]; i++)
      extendRightBM(&b[guide.hitsRight[s][i]]);
  }
  return n;
}

void makeProductBehaviour(State i, State j) 
{ 
  bdd_apply2_sequential 
    (aa1->ss[s].bddm, 
     BDD_ROOT(aa1->ss[s].bddm,
	      BEH(aa1->ss[s], paLeft(pairs[lSs], i), paLeft(pairs[rSs], j))), 
     aa2->ss[s].bddm, 
     BDD_ROOT(aa2->ss[s].bddm,
	      BEH(aa2->ss[s], paRight(pairs[lSs], i), paRight(pairs[rSs], j))), 
     res->ss[s].bddm, 
     fn_pair); 

  BM(b[s], i, j) = BDD_LAST_HANDLE(res->ss[s].bddm); 
}

GTA *gtaProduct(GTA *a1, GTA *a2, gtaProductType type) 
{
  State i, j;
  int done;
  char binfun[4];
  res = gtaMake();
  aa1 = a1;
  aa2 = a2;

  b = (BehaviourMatrix *) mem_alloc(sizeof(BehaviourMatrix)*guide.numSs);
  pairs = (PairArray *) mem_alloc(sizeof(PairArray)*guide.numSs);
  inverse = (PairHashTable *) mem_alloc(sizeof(PairHashTable)*guide.numSs);

  for (s = 0; s < guide.numSs; s++) {
    /* prepare bdd managers for sequential access (a1 and a2 must be minimal (?)) */
    unsigned estimate = 4 + 4 *
      (bdd_size(a1->ss[s].bddm) > bdd_size(a2->ss[s].bddm) ? 
       bdd_size(a1->ss[s].bddm) : bdd_size(a2->ss[s].bddm)); 
    
    res->ss[s].bddm = bdd_new_manager(estimate, 0);
    bdd_make_cache(res->ss[s].bddm, estimate, estimate/8 + 2); /* ??????? */
    
    /* initialize behaviour matrices */
    initBM(&b[s]);

    /* initialize a dynamic array for each state space */
    paInit(&pairs[s]);
    paInsert(&pairs[s], a1->ss[s].initial, a2->ss[s].initial);

    /* initialize hash tables */
    initPHT(&inverse[s], 7); /* 113 is such a nice prime */
    insertPHT(&inverse[s], a1->ss[s].initial, a2->ss[s].initial, 0);
  }

  /* while some behaviour-matrix needs some filling */
  do {
    done = 1;

    for (s = 0; s < guide.numSs; s++) {
      lSs = guide.muLeft[s];
      rSs = guide.muRight[s];

      if (b[s].lf < b[s].lu) {
	/* left-direction has been extended */
	int lu = b[s].lu, rf = b[s].rf;
	done = 0;

	for (i = b[s].lf; i < lu; i++)
	  for (j = 0; j < rf; j++)
	    /* make behaviour for the pairs */
	    makeProductBehaviour(i, j);

	b[s].lf = lu;
      }

      if (b[s].rf < b[s].ru) {
	/* right-direction has been extended */
	int ru = b[s].ru, lf = b[s].lf;
	done = 0;

	for (i = 0; i < lf; i++)
	  for (j = b[s].rf; j < ru; j++) 
	    /* make behaviour for the pairs */
	    makeProductBehaviour(i, j);
	
	b[s].rf = ru;
      }
    }
  } while (!done);

  /* move behaviour matrices */
  for (s = 0; s < guide.numSs; s++) {
    StateSpace *ss = &res->ss[s];
    ss->initial = 0; 
    ss->size = paSize(pairs[s]);
    ss->ls = b[s].ls;
    ss->rs = b[s].rs;
    ss->behaviour = b[s].m;
  }  

  /* set up final-status vector */
  binfun[0] = type&1; 
  binfun[1] = (type&2)>>1;
  binfun[2] = (type&4)>>2; 
  binfun[3] = (type&8)>>3;
  res->final = (int *) mem_alloc(sizeof(int)*res->ss[0].size);
  for (i = 0; i < res->ss[0].size; i++)
    res->final[i] = 
      ((a1->final[paLeft(pairs[0], i)] == 0 ||
	a2->final[paRight(pairs[0], i)] == 0)) ? 0 : 
    BOOL_TO_STATUS(binfun[STATUS_TO_BOOL(a1->final[paLeft(pairs[0], i)])*2 + 
			 STATUS_TO_BOOL(a2->final[paRight(pairs[0], i)])]);
  
  /* clean up */
  for (s = 0; s < guide.numSs; s++) {
    paFree(&pairs[s]);
    freePHT(&inverse[s]);
    bdd_kill_cache(res->ss[s].bddm);
  }
  mem_free(inverse);
  mem_free(pairs);
  mem_free(b);

  return res;
}
