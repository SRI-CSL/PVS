/* analyze_acceptance.c */
  
/*
 * MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
 *
 * Reproduction of all or part of this software is permitted for
 * educational or research use on condition that this copyright notice is
 * included in any copy. This software comes with no warranty of any
 * kind. In no event will BRICS be liable for any damages resulting from
 * use of this software.
 */

/* This package analyzes all states with respect to which kinds of
   final states (in state space 0) that are reachable */

#include <stdlib.h> 
#include "gta.h"

/* GLOBALS */

/* acceptance_statuses[d][p][s], where -1 <= s <= 1, is true if 
   and only if there is a labeled tree that makes 
   the GTA 1) enter a state at the root node that has status s
   and 2) enter the state p in a node assigned to state space d */
static boolean ***acceptance_statuses; 

/* touch_left[d][s] describes the set of all states s' in the left
   child space d' of d such that for some s'' in the right child space
   d'' and for some letter a, delta_d(s',s'')(a) = s */
/* touch_right is defined similarly */
static unsigned **touch_left_size, **touch_right_size;
static State ***touch_left, ***touch_right;

static unsigned  **touch_left_index, **touch_right_index;

static GTA *gta_global; 


/* UNPROCESSED AND MORE GLOBALS */

static State **unprocessed; /* unprocessed[d][i], i < next[d]
			       is a state in state space d */
static unsigned *next; /* next[d]= index of next state to be inserted in
			  unprocessed[d] */
static unsigned *done; /* done[d]= index of oldest state in d that
			  has been processed; done[d] <= next[d] */

static void unprocessed_push(SsId d, State p) 
{
  unsigned i;
  invariant(next[d] <= gta_global->ss[d].size);
  /* the following check makes it a quadratic algorithm in the state
     space size, but that probably doesn't matter too much since much
     else is quadratic */
  for (i = 0; i < next[d]; i++)
    if (unprocessed[d][i] == p) 
      return;
  unprocessed[d][next[d]++] = p;
}

static State unprocessed_get(SsId d) 
{
  invariant(done[d] <= next[d]);
  return unprocessed[d][done[d]++];
}

static boolean unprocessed_is_empty(SsId d) 
{
  invariant(done[d] <= next[d]);
  return (done[d] == next[d]);
} 

SsId current_d;
State current_left_state, current_right_state;

void leaf_function_insert_left(unsigned leaf_value)
{
  unsigned i = touch_left_index[current_d][leaf_value]++;
  touch_left[ current_d][leaf_value][i] = current_left_state;
}

void leaf_function_count_left(unsigned leaf_value)
{ 
  invariant(touch_left_size[current_d][leaf_value] <= 
	 gta_global->ss[current_d].ls);
  touch_left_size[current_d][leaf_value]++;
}

void leaf_function_insert_right(unsigned leaf_value)
{
  unsigned i = touch_right_index[current_d][leaf_value]++;
  touch_right[current_d][leaf_value][i] = current_right_state;
}

void leaf_function_count_right(unsigned leaf_value)
{
  invariant(touch_right_size[current_d][leaf_value] <= 
	 gta_global->ss[current_d].rs);
  touch_right_size[current_d][leaf_value]++;
}

void calculate_touch_arrays() 
{
  SsId d;
  State p, p_left, p_right;
  touch_left_size = (unsigned **) mem_alloc((sizeof (unsigned *)) *
					     guide.numSs);
  touch_right_size = (unsigned **) mem_alloc((sizeof (unsigned *)) *
					     guide.numSs);
  touch_left_index = (unsigned **) mem_alloc((sizeof (unsigned *)) *
					     guide.numSs);
  touch_right_index = (unsigned **) mem_alloc((sizeof (unsigned *)) *
					     guide.numSs);

  touch_left = (boolean ***) mem_alloc((sizeof (boolean **))* guide.numSs);
  touch_right = (boolean ***) mem_alloc((sizeof (boolean **))* guide.numSs);

  for (d = 0; d < guide.numSs; d++) {
    touch_left_size[d] = (unsigned *) mem_alloc((sizeof (unsigned)) *
						  gta_global->ss[d].size);
    touch_right_size[d] = (unsigned *) mem_alloc((sizeof (unsigned)) *
						   gta_global->ss[d].size);
    touch_left_index [d] = (unsigned *) mem_alloc((sizeof (unsigned)) *
						  gta_global->ss[d].size);
    touch_right_index [d] = (unsigned *) mem_alloc((sizeof (unsigned)) *
						   gta_global->ss[d].size);

    touch_left[d] = (boolean **) mem_alloc((sizeof (boolean *)) * gta_global->ss[d].size);
    touch_right[d] = (boolean **) mem_alloc((sizeof (boolean *)) * gta_global->ss[d].size);

    for (p = 0; p < gta_global->ss[d].size; p++) {
      touch_left_size[d][p] = 0;
      touch_right_size[d][p] = 0;
      touch_left_index[d][p] = 0;
      touch_right_index[d][p] = 0;
    }      
  }

  /* calculate sizes of the touch_left[d][p] arrays (and allocate) */

  /* For each left state, we sweep the row in the transition table
     while visiting all leaves reachable in behaviors of the row; for
     each leaf p reached, we insert the left state in the
     touch_left[d][p] array; note that we must clear out the
     apply-cache each time a new role is begun.  At this point in the
     algorithm, we are just calculating the sizes of the
     touch_left[d][p] arrays; the insertion proper is done in the next
     main loop */
  for (d = 0; d < guide.numSs; d++) {
    for (p_left = 0; p_left < gta_global->ss[d].ls; p_left++) {
      bdd_prepare_apply1(gta_global->ss[d].bddm); 
      current_d = d;
      current_left_state = p_left;
      for (p_right = 0; p_right < gta_global->ss[d].rs; p_right++)
	bdd_call_leafs(gta_global->ss[d].bddm, 
		       BDD_ROOT(gta_global->ss[d].bddm,
				BEH(gta_global->ss[d], p_left, p_right)),
		       &leaf_function_count_left);
    }
    for (p = 0; p < gta_global->ss[d].size; p++) 
      touch_left[d][p] = (State *) mem_alloc((sizeof (State)) *
					     touch_left_size[d][p]);
  }

  /* same loop as above, but now insert the elements into touch_left */
  for (d = 0; d < guide.numSs; d++) {
    for (p_left = 0; p_left < gta_global->ss[d].ls; p_left++) {
      bdd_prepare_apply1(gta_global->ss[d].bddm); 
      current_d = d;
      current_left_state = p_left;
      for (p_right = 0; p_right < gta_global->ss[d].rs; p_right++)
	bdd_call_leafs(gta_global->ss[d].bddm, 
		       BDD_ROOT(gta_global->ss[d].bddm,
				BEH(gta_global->ss[d], p_left, p_right)),
		       &leaf_function_insert_left);
    }
  }

  /* calculate sizes of the touch_right[d][p] arrays (and allocate) */
  for (d = 0; d < guide.numSs; d++) {
    for (p_right = 0; p_right < gta_global->ss[d].rs; p_right++) {
      bdd_prepare_apply1(gta_global->ss[d].bddm); 
      current_d = d;
      current_right_state = p_right;
      for (p_left = 0; p_left < gta_global->ss[d].ls; p_left++)
	bdd_call_leafs(gta_global->ss[d].bddm, 
		       BDD_ROOT(gta_global->ss[d].bddm,
				BEH(gta_global->ss[d], p_left, p_right)),
		       &leaf_function_count_right); 
    }
    for (p = 0; p < gta_global->ss[d].size; p++) 
      touch_right[d][p] = (State *) mem_alloc((sizeof (State)) *
					     touch_right_size[d][p]);
  }

  /* same loop as above, but now insert the elements into touch_right */
  for (d = 0; d < guide.numSs; d++) {
    for (p_right = 0; p_right < gta_global->ss[d].rs; p_right++) {
      bdd_prepare_apply1(gta_global->ss[d].bddm); 
      current_d = d;
      current_right_state = p_right;
      for (p_left = 0; p_left < gta_global->ss[d].ls; p_left++)
	bdd_call_leafs(gta_global->ss[d].bddm, 
		       BDD_ROOT(gta_global->ss[d].bddm,
				BEH(gta_global->ss[d], p_left, p_right)),
		       &leaf_function_insert_right);
    }
  }
}

void gtaFreeInheritedAcceptance(boolean ***a) {
  SsId d;
  State p;
  for (d = 0; d < guide.numSs; d++) {
    for (p = 0; a[d][p]; p++) 
      free(a[d][p]-1);
    free(a[d]);
  }
  free(a);
}

boolean ***gtaCalcInheritedAcceptance(GTA *gta) 
{   
  SsId d;
  State p;

  gta_global = gta;
  acceptance_statuses = (boolean ***) 
    mem_alloc(sizeof (boolean **) * guide.numSs);
  

  unprocessed = (State **) mem_alloc(sizeof(State *) * guide.numSs);
  next = (unsigned *) mem_alloc(sizeof (unsigned) * guide.numSs);
  done = (unsigned *) mem_alloc(sizeof (unsigned) * guide.numSs);

  calculate_touch_arrays();

  /* initialize acceptance_statuses to the empty set everywhere except in
     state space d == 0, where the accept status is included in
     acceptance_statuses (so that it becomes a singleton set) */
  for (d = 0; d < guide.numSs; d++) {
    next[d] = 0; 
    done[d] = 0;
    unprocessed[d] = (State *) 
      mem_alloc(sizeof (State) * gta->ss[d].size);
    acceptance_statuses[d] = 
      (boolean **) mem_alloc(sizeof (boolean *) * (gta->ss[d].size + 1));
    acceptance_statuses[d][gta->ss[d].size] = 0; /* end-of-array marker */
    for (p = 0; p < gta->ss[d].size; p++) {
      int s;
      acceptance_statuses[d][p] = 
	((boolean *) mem_alloc(sizeof (boolean) * 3)) + 1; 
      /* "+ 1" above is a pointer-arithmetic offset so that we can index
	 into the array using indices -1, 0, +1 */
      for (s = -1; s <= 1; s++)
	acceptance_statuses[d][p][s] = 0;
      if (d == 0) {
	invariant(gta->final[p] <= 1 && gta->final[p] >=-1);
	acceptance_statuses[d][p][gta->final[p]] = 1;
	unprocessed_push(d,p);
      }
    }
  }
  { 
    SsId d_hat, d_left, d_right;
    State p_hat, p_left, p_right;
    unsigned i;
    int s;
    d_hat = 0;
    while (d_hat < guide.numSs)
      if (!unprocessed_is_empty(d_hat)) {
	p_hat = unprocessed_get(d_hat);

	/* do left part  */
	d_left = guide.muLeft[d_hat];
	for (i = 0; i < touch_left_size[d_hat][p_hat]; i++) {
	  p_left = touch_left[d_hat][p_hat][i];
	  for (s = -1; s <= 1; s++) 
	    /* check to see whether s is a new possible status reachable 
	       from state p_left in state space d_left */
	    if (acceptance_statuses[d_hat][p_hat][s] && 
		!acceptance_statuses[d_left][p_left][s]) {
	      /* and if so, then mark it as such and push it onto stack */
	      acceptance_statuses[d_left][p_left][s] = 1;
	      unprocessed_push(d_left, p_left);
	    }
	}

	/* do right part */
	d_right = guide.muRight[d_hat];
	for (i = 0; i < touch_right_size[d_hat][p_hat]; i++) {
	  p_right = touch_right[d_hat][p_hat][i];
	  for (s = -1; s <= 1; s++) 
	    /* check to see whether s is a new possible status reachable 
	       from state p_right in state space d_right */
	    if (acceptance_statuses[d_hat][p_hat][s] && 
		!acceptance_statuses[d_right][p_right][s]) {
	      /* and if so, then mark it as such and push it onto stack */
	      acceptance_statuses[d_right][p_right][s] = 1;
	      unprocessed_push(d_right, p_right);
	    }
	}
	/* done with processing p_hat */
	d_hat = 0; /* we try the lowest numbered state space first */
      }
      else /* d_hat seems to be done */
	d_hat++;

    /* while loop ends here */
  }  

  for (d = 0; d < guide.numSs; d++) {
    free(unprocessed[d]);
  }
  free(unprocessed);
  free(next);
  free(done);

  for (d = 0; d < guide.numSs; d++) {
    for (p = 0; p < gta_global->ss[d].size; p++) {
      free(touch_left[d][p]);
      free(touch_right[d][p]);
    } 
    free(touch_left[d]);
    free(touch_right[d]);
    free(touch_left_size[d]);
    free(touch_right_size[d]);
    free(touch_left_index[d]);
    free(touch_right_index[d]);
  }
  free(touch_left);
  free(touch_right);
  free(touch_left_size);
  free(touch_right_size);
  free(touch_left_index);
  free(touch_right_index);

  return acceptance_statuses;
}
  
