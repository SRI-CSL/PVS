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
#include "dfa.h"
#include "bdd.h"
#include "../BDD/hash.h"
#include "../Mem/mem.h"

#define STATUS_TO_BOOL(s) \
((s == -1)? 0: 1)

#define BOOL_TO_STATUS(s) \
((s == 0)? -1 : 1)

struct list_ {
  unsigned li1;
  unsigned li2;
  struct list_ *next;  
};

typedef struct list_ *list;

list new_list(unsigned i1, unsigned i2, list nxt)
{  
  list l = mem_alloc(sizeof *l);

  l->li1 = i1;
  l->li2 = i2;
  l->next = nxt;
  
  return(l);
}

/* These global because used in prod_term_fn.  */
static int last_state;
static list qst, qh, qt;
static hash_tab htbl;  

unsigned prod_term_fn(unsigned  p, unsigned q)
{    
  int res;

  if ( (res = (int) (long) lookup_in_hash_tab(htbl, p, q)) )
    /* res = 0 or id+1 */
    return (--res);
  else {
    insert_in_hash_tab(htbl,  p, 
		       q, (void *) (long) (res = ++last_state));
    qt->next = new_list(p, q, (list) 0);
    qt = qt->next;

    return (--res);
  }
}

 
/*insert a loop for the product state (p, q) */
inline void make_loop (bdd_manager *bddm, unsigned  p, unsigned q) {
  int res;
  res = (int) (long) lookup_in_hash_tab(htbl, p, q);
  invariant(res);
  /* res = 0 or id+1 */
  (--res);
  /* make the next entry in bdd_roots(bddm) a leaf with value res;
     thus a loop is created */
  invariant(bdd_roots_length(bddm) == (unsigned int) res);
#ifdef _AUTOMATON_HASHED_IN_PRODUCT_
  bdd_find_leaf_hashed_add_root(bddm, res);
#else
  BDD_ADD_ROOT(bddm, bdd_find_leaf_sequential(bddm, res));
#endif
}

inline int lookup_binfun(int x, int y, char *binfun) {
  return binfun[2*x + y];
}

inline int is_loop (bdd_manager *bddm, unsigned p, unsigned w) {
  return (bdd_is_leaf (bddm, w) && (bdd_leaf_value (bddm, w) == p));
}

/* return 1 if a loop involving an accepting state can be made; return
   -1 if a loop with a rejecting state can be made; return
   0 if loop with bottom can be made, otherwise return 2 */
inline int make_a_loop_status (int is_loop_p, int status_p,
			int is_loop_q, int status_q,
			char *binfun) {
  if (is_loop_p) {
    /* if the accept status of the second argument doesn't
       impact the status of the product automaton, then 
       a loop can made */
    if (status_p == 0) /* product state is bottom */
      return 0;
    /* so, status_p is not bottom; however, the resulting automaton
       must still correctly distingusih between bottom and non-bottom
       states */
    return 2; /* note: in case both are loops, we don't need to
		   create a resulting loop explicitly */
  } else 
    if (is_loop_q) 
      if (status_q == 0)
	return 0;
  return 2;
}

DFA *dfaProduct(DFA* a1, DFA* a2, dfaProductType ff) 
{
  DFA *b;
  int i;
  unsigned *root_ptr;
  char binfun[4];
  int make_a_loop;
  
  unsigned size_estimate = 4 + 4 *
    (mona_bdd_size(a1->bddm) > mona_bdd_size(a2->bddm) ? 
     mona_bdd_size(a1->bddm) : mona_bdd_size(a2->bddm)); 
  
  bdd_manager *bddm; 
  
/* #define _AUTOMATON_HASHED_IN_PRODUCT_
 */

#ifdef _AUTOMATON_HASHED_IN_PRODUCT_
  /*prepare hashed access */
  
  bddm = bdd_new_manager(size_estimate, size_estimate/8 + 2);
  bdd_make_cache(bddm, size_estimate, size_estimate/8 + 2);    
  bddm->cache_erase_on_doubling = TRUE ; 
#else
  /*prepare sequential access*/
  bddm = bdd_new_manager(size_estimate, 0);
  bdd_make_cache(bddm, size_estimate, size_estimate/8 + 2); 
#endif
  
  binfun[0] = ff&1; binfun[1] = (ff&2)>>1;     /* The binary function */
  binfun[2] = (ff&4)>>2; binfun[3] = (ff&8)>>3;
  
  qst = qh = qt = new_list(a1->s, a2->s, (list) 0);
  htbl = new_hash_tab(&hash2, &eq2);
  insert_in_hash_tab(htbl, a1->s, a2->s, (void *) 1);
  last_state = 1;  /* Careful here! Bdd's start at 0, hashtbl at 1 */
  
  while(qh) {      /* Our main loop, nice and tight */
    make_a_loop = make_a_loop_status(is_loop(a1->bddm, qh->li1, 
					     a1->q[qh->li1]),
				     a1->f[qh->li1],
				     is_loop(a2->bddm, qh->li2,
					     a2->q[qh->li2]),
				     a2->f[qh->li2],
				     binfun);
    if  (make_a_loop != 2) 
      make_loop(bddm, qh->li1, qh->li2);
    else {
#ifdef _AUTOMATON_HASHED_IN_PRODUCT_
      (void) bdd_apply2_hashed (a1->bddm, a1->q[qh->li1], 
				a2->bddm, a2->q[qh->li2],
				bddm,
				&prod_term_fn);
#else       
      (void) bdd_apply2_sequential (a1->bddm, a1->q[qh->li1], 
				    a2->bddm, a2->q[qh->li2], 
				    bddm,
				    &prod_term_fn);
#endif	     
    }
    qh = qh->next;
  }
  b = dfaMakeNoBddm(last_state);   /* Return the result */
  b->s = 0;             /* Always first on list */
  b->bddm = bddm;
  for (i=0, root_ptr = bdd_roots(bddm); 
       i < last_state; root_ptr++, i++) {
    list qnxt;
    
    b->q[i] = *root_ptr;
    b->f[i] = ((a1->f[qst->li1] != 0) && (a2->f[qst->li2] != 0)) ?
      /* both states are non-bottom, use "binfun" */
      BOOL_TO_STATUS(binfun[STATUS_TO_BOOL(a1->f[qst->li1])*2 
			   + STATUS_TO_BOOL(a2->f[qst->li2])]) :
      /* at least one is bottom */
      0;
    qnxt = qst->next;
    mem_free(qst);      /* Free the list */
    qst = qnxt;
  }
  
  free_hash_tab(htbl);
  bdd_update_statistics(bddm, (unsigned) PRODUCT);
  bdd_kill_cache(b->bddm);
  return(b);
  
}
