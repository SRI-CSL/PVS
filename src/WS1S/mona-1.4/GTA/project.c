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
#include "dyn.h" /* BehaviourMatrix */
#include "projset.h" /* StateSet */
#include "subsets.h" /* Subsets */

GTA *orig, *res; /* original and result GTA */
SsId s, lSs, rSs; /* current, left and right state spaces */

StateSet *unproc, *initial; /* for quotient */

BehaviourMatrix *b; /* extendable behaviour matrix */
Subsets *sets; /* subset containers */

State *init; /* initial states */
int *final; /* final status sequence */
unsigned finalAllocated; /* allocated size of 'final' */

/* quotient auxiliary functions */

State read0X0(bdd_manager *bddm, bdd_ptr p, unsigned idx, unsigned choice) {
  if (bdd_is_leaf(bddm, p)) 
    return bdd_leaf_value(bddm, p);
  else 
    if (bdd_ifindex(bddm, p) == idx) {
      if (choice)
        return read0X0(bddm, bdd_then(bddm, p), idx, choice);
      else
        return read0X0(bddm, bdd_else(bddm, p), idx, choice);
    }
    else
      return read0X0(bddm, bdd_else(bddm, p), idx, choice);
}

void zeroPathStates(SsId d, State i, State j, unsigned idx)
{
  State z0, z1;
  bdd_ptr pp = BDD_ROOT(orig->ss[d].bddm,
			orig->ss[d].behaviour[i*orig->ss[d].rs + j]);

  z0 = read0X0(orig->ss[d].bddm, pp, idx, 0);
  z1 = read0X0(orig->ss[d].bddm, pp, idx, 1);
  
  if (!setExists(&initial[d], z0)) {
    setInsert(&unproc[d], z0);
    setInsert(&initial[d], z0);
  }
  if (z0 != z1 && 
      !setExists(&initial[d], z1)) {
    setInsert(&unproc[d], z1);
    setInsert(&initial[d], z1);
  }
}

/* project auxiliary functions */

unsigned fn_union(unsigned v1, unsigned v2)
{
  unsigned n;

  /* make new state for the union set if it's new */
  if (!ssLookupAndInsert(&sets[s], v1, v2, &n)) {
    SsId i;

    if (s == 0) {
      /* set final status for new state */
      if (n == finalAllocated) {
	finalAllocated = finalAllocated*2 + 5;
	final = (int *) mem_resize(final, sizeof(int)*finalAllocated);
      }
      final[n] = 
	(final[v1] == 0 && final[v2] == 0) ? 0 :
	((final[v1] == 1 || final[v2] == 1) ? 1 : -1);
    }

    /* extend matrices */
    for (i = 0; i < guide.numHitsLeft[s]; i++)
      extendLeftBM(&b[guide.hitsLeft[s][i]]);
    for (i = 0; i < guide.numHitsRight[s]; i++)
      extendRightBM(&b[guide.hitsRight[s][i]]);

  }
  return n;
}

void makeProjectBehaviour(State i, State j)
{
  if (i >= orig->ss[lSs].size) {
    if (j >= orig->ss[rSs].size) { /* {i1,i2}x{j1,j2} */
      State i1, i2, j1, j2;
      bdd_handle t1, t2;
      ssGetComponents(&sets[lSs], i, &i1, &i2);
      ssGetComponents(&sets[rSs], j, &j1, &j2);
      bdd_apply2_hashed
	(res->ss[s].bddm, BDD_ROOT(res->ss[s].bddm, BM(b[s], i1, j1)),
	 res->ss[s].bddm, BDD_ROOT(res->ss[s].bddm, BM(b[s], i1, j2)),
	 res->ss[s].bddm, fn_union);
      t1 = BDD_LAST_HANDLE(res->ss[s].bddm);
      bdd_apply2_hashed
	(res->ss[s].bddm, BDD_ROOT(res->ss[s].bddm, BM(b[s], i2, j1)),
	 res->ss[s].bddm, BDD_ROOT(res->ss[s].bddm, BM(b[s], i2, j2)),
	 res->ss[s].bddm, fn_union);
      t2 = BDD_LAST_HANDLE(res->ss[s].bddm);
      bdd_apply2_hashed
	(res->ss[s].bddm, BDD_ROOT(res->ss[s].bddm, t1),
	 res->ss[s].bddm, BDD_ROOT(res->ss[s].bddm, t2),
	 res->ss[s].bddm, fn_union); /***this case could be handled differently!!***/
    }
    else { /* {i1,i2}x{j} */
      State i1, i2;
      ssGetComponents(&sets[lSs], i, &i1, &i2);
      bdd_apply2_hashed
	(res->ss[s].bddm, BDD_ROOT(res->ss[s].bddm, BM(b[s], i1, j)),
	 res->ss[s].bddm, BDD_ROOT(res->ss[s].bddm, BM(b[s], i2, j)),
	 res->ss[s].bddm, fn_union);
    }
  }
  else { /* {i}x{j1,j2} */
      State j1, j2;
      ssGetComponents(&sets[rSs], j, &j1, &j2);
      bdd_apply2_hashed
	(res->ss[s].bddm, BDD_ROOT(res->ss[s].bddm, BM(b[s], i, j1)),
	 res->ss[s].bddm, BDD_ROOT(res->ss[s].bddm, BM(b[s], i, j2)),
	 res->ss[s].bddm, fn_union);
  } /* ({i}x{j} was filled by bdd_project) */

  BM(b[s], i, j) = BDD_LAST_HANDLE(res->ss[s].bddm);
}

/* main function */

GTA *gtaQuotientAndProject(GTA *g, unsigned idx, int quotient)
{
  unsigned i, j;
  int done;

  orig = g;
  res = gtaMake();

  /* QUOTIENT */

  /* initialize unprocessed and initial sets */
  unproc = (StateSet *) mem_alloc(sizeof(StateSet)*guide.numSs);
  initial = (StateSet *) mem_alloc(sizeof(StateSet)*guide.numSs);
  for (s = 0; s < guide.numSs; s++) {
    setInit(&unproc[s], g->ss[s].size);
    setInit(&initial[s], g->ss[s].size);
    setInsert(&unproc[s], g->ss[s].initial);
    setInsert(&initial[s], g->ss[s].initial);
  }

  if (quotient)
    /* iterate until no more unprocessed */
    do {
      done = 1;
      for (s = 0; s < guide.numSs; s++) 
	if (!setEmpty(&unproc[s])) {
	  
	  /* pick an unprocessed state q_hat in state space s */
	  State q_hat = setRemoveOne(&unproc[s]);
	  done = 0;
	  
	  /* for each d that has s as left successor */
	  for (i = 0; i < guide.numHitsLeft[s]; i++) {
	    SsId d = guide.hitsLeft[s][i];
	    SsId d2 = guide.muRight[d];
	    
	    /* for each q2 in initial set of right successor of d */
	    for (j = 0; j < setSize(&initial[d2]); j++) {
	      State q2 = setRead(&initial[d2], j);
	      
	      /* find zero path states and extend sets */
	      zeroPathStates(d, q_hat, q2, idx);
	    }
	  }
	  
	  /* for each d that has s as right successor */
	  for (i = 0; i < guide.numHitsRight[s]; i++) {
	    SsId d = guide.hitsRight[s][i];
	    SsId d1 = guide.muLeft[d];
	    
	    /* for each q1 in initial set of left successor of d */
	    for (j = 0; j < setSize(&initial[d1]); j++) {
	      State q1 = setRead(&initial[d1], j);
	      
	      /* find zero path states and extend sets */
	      zeroPathStates(d, q1, q_hat, idx);
	    }
	  }
	}
    } while (!done);

  /* PROJECT */

  /* initialize */
  b = (BehaviourMatrix *) mem_alloc(sizeof(BehaviourMatrix)*guide.numSs);
  sets = (Subsets *) mem_alloc(sizeof(Subsets)*guide.numSs);

  final = (int *) mem_alloc(sizeof(int)*g->ss[0].size);
  finalAllocated = g->ss[0].size;
  for (i = 0; i < g->ss[0].size; i++)
    final[i] = g->final[i];

  for (s = 0; s < guide.numSs; s++) {
    unsigned estimate = 2 * bdd_size(g->ss[s].bddm);

    res->ss[s].bddm = bdd_new_manager(estimate, estimate/8 + 2);
    bdd_make_cache(res->ss[s].bddm, estimate, estimate/8 + 2);

    initBMtoSize(&b[s], 
		 g->ss[guide.muLeft[s]].size,
		 g->ss[guide.muRight[s]].size);
    ssInit(&sets[s], g->ss[s].size, 7);
  }
  
  /* make nondeterministic automaton and prepare determinization */
  for (s = 0; s < guide.numSs; s++) {
    for (i = 0; i < g->ss[guide.muLeft[s]].size; i++)
      for (j = 0; j < g->ss[guide.muRight[s]].size; j++) {
	bdd_project(g->ss[s].bddm,
		    BDD_ROOT(g->ss[s].bddm,
			     g->ss[s].behaviour[i*g->ss[s].rs+j]),
		    idx,
		    res->ss[s].bddm,
		    fn_union);
	BM(b[s], i, j) = BDD_LAST_HANDLE(res->ss[s].bddm);
      }
    b[s].lf = g->ss[guide.muLeft[s]].size;
    b[s].rf = g->ss[guide.muRight[s]].size;
  }

  /* clear BDD cache */
  for (s = 0; s < guide.numSs; s++) {
    unsigned estimate = 2 * bdd_size(g->ss[s].bddm);

    bdd_kill_cache(res->ss[s].bddm);
    bdd_make_cache(res->ss[s].bddm, estimate, estimate/8 + 2);
    res->ss[s].bddm->cache_erase_on_doubling = TRUE; /* ???? */
    bdd_prepare_apply1(res->ss[s].bddm); /* ??????????????????????????????? */
  }
  
  /* insert initial sets as new states */
  init = (State *) mem_alloc(sizeof(State)*guide.numSs);
  for (s = 0; s < guide.numSs; s++) 
    if (setSize(&initial[s]) >= 2) { /***initial sets could be unfolded better!!!***/
      unsigned i, s1, s2;
      
      /* make new state for the first two elements */
      s1 = fn_union(setRead(&initial[s], 0), setRead(&initial[s], 1));

      /* unfold the rest and make new states */
      for (i = 2; i < setSize(&initial[s]); i++) {
	s2 = setRead(&initial[s], i);
	s1 = fn_union(s1, s2);
      }
      init[s] = s1;
    }
    else
      init[s] = setRead(&initial[s], 0);
  
  /* determinize by iterating until all new behaviour is filled */
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
	    /* make behaviour for the set */
	    makeProjectBehaviour(i, j);
	
	b[s].lf = lu;
      }
      
      if (b[s].rf < b[s].ru) {
	/* right-direction has been extended */
	int ru = b[s].ru, lf = b[s].lf;
	done = 0;
	
	for (i = 0; i < lf; i++)
	  for (j = b[s].rf; j < ru; j++)
	    /* make behaviour for the set */
	    makeProjectBehaviour(i, j);
	
	b[s].rf = ru;
      }
    }
  } while (!done);

  /* move behaviour to result automaton */  
  for (s = 0; s < guide.numSs; s++) {
    StateSpace *ss = &res->ss[s];

    ss->initial = init[s]; 
    ss->size = ssSize(&sets[s]);
    ss->ls = b[s].ls;
    ss->rs = b[s].rs;
    ss->behaviour = b[s].m;
  }

  /* set final status */
  res->final = (int *) mem_alloc(sizeof(int)*res->ss[0].size);
  for (i = 0; i < res->ss[0].size; i++)
    res->final[i] = final[i];

  /* clean up */
  for (s = 0; s < guide.numSs; s++) {
    ssFree(&sets[s]);
    setFree(&unproc[s]);
    setFree(&initial[s]);
  }
  mem_free(sets);
  mem_free(b);
  mem_free(final);
  mem_free(unproc);
  mem_free(initial);
  mem_free(init);

  return gtaReachable(res);
}
