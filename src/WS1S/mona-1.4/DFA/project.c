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

#include "dfa.h"
#include "../BDD/hash.h"
#include "../Mem/mem.h"

static bdd_manager *bddm_res;

#define SET_BDD_NOT_CALCULATED (unsigned)-1

struct set {
   int size;
   int *elements;
   unsigned sq;     /* bddm->SEQUENTIAL_LIST(roots)[sq]  is the
		       BDD node of the subset state; if equal
		       SET_BDD_NOT_CALCULATED then not calculated yet */
   int decomp1, decomp2; /* union these to get this set */
   int permanent;        /* state in final automata, -1 = none */
}; 
   
static int n_ssets;
static struct set *ssets;
static int next_sset;
static hash_tab htbl_set;

void init_ssets(int sz)
{
   n_ssets = sz;
   ssets = mem_alloc((sizeof *ssets) * sz);
   next_sset = 0;
}

int make_sset(int sz, int *elem,  unsigned sq, int d1, int d2)
{
  if (next_sset == n_ssets) {
    struct set *new_ssets = mem_alloc((sizeof *ssets) * n_ssets * 2);
    
    mem_copy(new_ssets, ssets, (sizeof *new_ssets) * n_ssets);
    mem_free(ssets);
    ssets = new_ssets;
    n_ssets *= 2;
  }
  ssets[next_sset].size = sz;
  ssets[next_sset].elements = elem;
  ssets[next_sset].sq = sq;
  ssets[next_sset].decomp1 = d1;
  ssets[next_sset].decomp2 = d2;
  ssets[next_sset].permanent = -1;
  insert_in_hash_tab(htbl_set, (long)elem, 0, (void *)(next_sset+1));  
  /* htbl maps to ++id, since 0 = not_found */ 

  return(next_sset++);
}       


struct sslist_ {
  int sset_id;
  struct sslist_ *next;  
};

typedef struct sslist_ *sslist;

sslist new_sslist(int si, sslist nxt)
{  
  sslist sl = mem_alloc(sizeof *sl);
  
  sl->sset_id = si;
  sl->next = nxt;
  
  return(sl);
}


/* These global because used in proj_term.  */
static sslist lst, lh, lt;

/* Fn to create pairs */
unsigned proj_term1(unsigned state1, unsigned  state2)
{  
  int res;
  int *s;
  int size;

  if (state1 == state2) {
    size = 1;
    s = mem_alloc((sizeof *s) * 2);
    s[0] = state1; s[1] = -1;         
  } 
  else {
    size = 2;
    s = mem_alloc((sizeof *s) * 3);
    if(state1<state2) {
      s[0] = state1;
      s[1] = state2; 
      s[2] = -1;
    }
    else {
      s[0] = state2; 
      s[1] = state1; 
      s[2] = -1;
    }
  }
   
  /* res = 0 or id+1 */
  if ( (res = (int) lookup_in_hash_tab(htbl_set, (long) s, 0)) ) {
    mem_free(s); /* it was already there */  
    return (--res);
  }
  else {
    res = make_sset(size, s, SET_BDD_NOT_CALCULATED, state1, state2); /* optimize if equal? */
    return (res);
  }
}


/* Fn to union leaves */
bdd_ptr proj_term2(unsigned set_index1,  unsigned set_index2)
{  
  int res;
  int *s;
  struct set *ss1, *ss2;
  int *e1, *e2, *e3;
  ss1 = &(ssets[set_index1]);
  ss2 = &(ssets[set_index2]);
  s = mem_alloc((ss1->size + ss2->size + 1) * (sizeof *s));
  
  /* Union the sets */      
  for (e1 = ss1->elements, e2 = ss2->elements, e3 = s; 
      (*e1 >= 0) && (*e2 >= 0);) {
    if (*e1 < *e2)
      *e3++ = *e1++;
    else if (*e1 == *e2) {
      *e3++ = *e1++; 
      e2++;
    }
    else
	*e3++ = *e2++;
  }
  if(*e1 >= 0) 
    do {
      *e3++ = *e1++;
    } while(*e1 >= 0);
  else if (*e2 >= 0) 
    do {
      *e3++ = *e2++;
    } while(*e2 >= 0);
  
  *e3 = -1;   /* Terminate the new set */
  
  /* res = 0 or id+1 */
  if ( (res = (int) lookup_in_hash_tab(htbl_set, (long) s, 0)) ) {
    mem_free(s); /* it was already there */
    return (--res);
  }
  else {
    res = make_sset((e3-s), s, SET_BDD_NOT_CALCULATED, set_index1, set_index2);
    return (res);
  }
}

static int next_state;

/* Fn to insert leaves and return permanent "q" */
bdd_ptr proj_term3(unsigned p)
{ 
  if(ssets[p].permanent < 0) {
    lt->next = new_sslist(p, 0);   /* Put in queue */
    lt = lt->next;
    ssets[p].permanent = next_state++;
  }

  return (ssets[p].permanent);
}

unsigned eval_bdd(int ss)
{
  unsigned root1, root2;
  
  if (ssets[ss].sq == SET_BDD_NOT_CALCULATED) {
    root1 = eval_bdd(ssets[ss].decomp1);
    root2 = eval_bdd(ssets[ss].decomp2);
    (void) bdd_apply2_hashed(bddm_res, bdd_roots(bddm_res)[root1],
			     bddm_res, bdd_roots(bddm_res)[root2],
			     bddm_res, &proj_term2);
    ssets[ss].sq = bdd_roots_length(bddm_res) - 1;
  }

  return(ssets[ss].sq);
} 

DFA *dfaProject(DFA *a, unsigned var_index) 
{
  int i,*e; 
  DFA *res;
  sslist lnxt;  
  unsigned size_estimate = 2 * bdd_size(a->bddm);
  
  bddm_res = bdd_new_manager(size_estimate, size_estimate/8 + 2);
  bdd_make_cache(bddm_res, size_estimate, size_estimate/8 + 2);    
  bddm_res->cache_erase_on_doubling = TRUE;
  
  init_ssets(a->ns * 2);
  htbl_set = new_hash_tab(hashlong, eqlong);
  next_state = 0; 
  
  for(i = 0; i < a->ns; i++) {  /* Allocate singletons, ssets[i] = {i} */
    int *s = mem_alloc(2 * (sizeof *s));
    
    s[0] = i; s[1] = -1;
    make_sset(1, s, SET_BDD_NOT_CALCULATED, -1, -1);
  }

  for (i = 0; i < a->ns; i++) {  /* Update bdd's */  
    (void) bdd_project(a->bddm, a->q[i], var_index, bddm_res, &proj_term1);
    ssets[i].sq = bdd_roots_length(bddm_res) - 1; /* bdd_roots_length(bddm_res) - 1 == i */
    /* bdd_roots(bddm_res)[ssets[i].sq] now contains 
       place where a node index is to be found*/
  } 
  
  /* Create a list of reachable sets. */
  lst = lh = lt = new_sslist(a->s, 0);   /* start singleton */
  ssets[a->s].permanent = next_state++;  /* Should be 0 */
  {
    unsigned root_place;
    bdd_manager *bddm_res_ = bdd_new_manager(size_estimate,
					     size_estimate / 8 + 2);
    bdd_make_cache(bddm_res_, size_estimate, size_estimate /8 + 2);
    bdd_kill_cache(bddm_res);
    bdd_make_cache(bddm_res, size_estimate, size_estimate/8 + 2);    
    bddm_res->cache_erase_on_doubling = TRUE;
    
    bdd_prepare_apply1(bddm_res);
    while (lh) {
      root_place = eval_bdd(lh->sset_id);
      /* Insert leaves */    
      (void) bdd_apply1(bddm_res, bdd_roots(bddm_res)[root_place], bddm_res_, &proj_term3);
      /*evaluate bdd_roots(bddm_res) at each iteration since bdd_apply1 is called*/
      lh = lh -> next;
    }
   
    {
      unsigned *new_roots;
      
      res = dfaMakeNoBddm(next_state);
      res->bddm = bddm_res_;
      new_roots = bdd_roots(bddm_res_);
      
      for (i = 0; i < next_state; i++) {   /* Walk through list */
	int non_bottom_found = 0;
	int plus_one_found = 0;
	res->q[i] = new_roots[i];
	for (e = ssets[lst->sset_id].elements; *e >= 0; e++) {
	    non_bottom_found += (a->f[*e] != 0);
	    plus_one_found += (a->f[*e] == 1);
	}
	if (!non_bottom_found)
	  res->f[i] = 0;
	else
	  if (plus_one_found)
	    res->f[i] = 1;
	  else
	    res->f[i] = -1;
	res->s = ssets[a->s].permanent;  /* Move to out of loop */
	
	lnxt = lst -> next;
	mem_free(lst);          /* Free the list */
	lst = lnxt;
      }
    
      for(i = 0; i < next_sset; i++) 
	mem_free(ssets[i].elements);
      
      mem_free(ssets);
      free_hash_tab(htbl_set);  
      bdd_update_statistics(bddm_res, (unsigned)PROJECT);
      bdd_update_statistics(bddm_res_, (unsigned)PROJECT);
      bdd_kill_manager(bddm_res);
      return(res);
    }
  }
}
