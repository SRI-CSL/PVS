/*bdd_double.c*/
 
/*
 * MONA is Copyright (C) 1997 BRICS. All rights reserved. 
 *
 * Reproduction of all or part of this software is permitted for
 * educational or research use on condition that this copyright notice is
 * included in any copy. This software comes with no warranty of any
 * kind. In no event will BRICS be liable for any damages resulting from
 * use of this software.
 */

#include "bdd.h"
#include "bdd_internal.h"
#include "long_mem/memuser.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*DOUBLE_TABLE stuff*/

void double_table_sequential(bdd_manager *bddm) {
  bddm->table_total_size += bddm->table_size;
  bddm->table_size += bddm->table_size;
  
/*  printf("Doubling table to: Size %u Total %u\n", bddm->table_size, bddm->table_total_size); */

  bddm->node_table = mem_resize_block(bddm->node_table,
				      (SIZE_T)(sizeof (bdd_record)) * 
				      (bddm->table_total_size));
  bddm->table_log_size++;
#ifdef _BDD_STAT_
  bddm->number_double++;
#endif
}



unsigned double_leaf_fn(unsigned value) {
  return (value);
}

static bdd_manager *old_bddm;

unsigned get_new_r(unsigned r) {
  if (r == 0 || r == BDD_UNDEF) {
    return (r);
  }
  return (old_bddm->node_table[r].mark);
}

void double_table_and_cache_hashed(bdd_manager *bddm,
				   unsigned* some_roots,
				   void (*update_fn)(unsigned (*new_place)(unsigned node)),
				   unsigned *p_of_find, unsigned *q_of_find,
				   boolean rehash_p_and_q) {
  unsigned *p;  

  old_bddm = mem_get_block((SIZE_T) sizeof (bdd_manager));
  *old_bddm = *bddm;

  /*make new bigger table, but only if a bigger one is possible */
  invariant(bddm->table_total_size < BDD_MAX_TOTAL_TABLE_SIZE);
  bddm->table_log_size++;
  bddm->table_size *= 2;
  bddm->table_overflow_increment *= 2;
  {
    unsigned desired_size = bddm->table_size + BDD_NUMBER_OF_BINS + 
      bddm->table_overflow_increment;
    bddm->table_total_size = (desired_size <= BDD_MAX_TOTAL_TABLE_SIZE)?
      desired_size: BDD_MAX_TOTAL_TABLE_SIZE;
  }
  bddm->node_table = (bdd_record*) 
      mem_get_block( (SIZE_T)
		    bddm->table_total_size
		    * (sizeof (bdd_record)));
  bddm->table_mask =   bddm->table_size - BDD_NUMBER_OF_BINS; 

  bddm->table_double_trigger *= 2;
  bddm->table_overflow =  bddm->table_size + BDD_NUMBER_OF_BINS; 
#ifdef _BDD_STAT_
  bddm->number_double++;
#endif

  /* initialize to unused */
  bddm->table_elements = 0;
  mem_zero(&bddm->node_table[BDD_NUMBER_OF_BINS], (SIZE_T)
	   bddm->table_size * (sizeof (bdd_record)));

  /* initialize bddm roots to the empty list, this new list will
     contain the rehashed addresses of old_bddm->roots*/
  MAKE_SEQUENTIAL_LIST(bddm->roots, unsigned, 1024);
  
  /*now rehash all nodes reachable from the old roots; we must be sure
    that the apply1 operation does not entail doubling of bddm node
    table: this is achieved by our having just doubled the size of the
    table*/

  

  bdd_prepare_apply1(old_bddm);

  for (p = SEQUENTIAL_LIST(old_bddm->roots); *p ;  p++) {
    bdd_apply1(old_bddm, *p, bddm, &double_leaf_fn);    
  }
 
  /*also make sure to rehash portion that is accessible from some_roots*/ 
  for (p = some_roots; *p;  p++) {
    if (*p != BDD_UNDEF)
	*p = bdd_apply1_dont_add_roots(old_bddm, *p, bddm, &double_leaf_fn);    
  }

  /*and fix values p_of_find and q_of_find if indicated*/
  if (rehash_p_and_q) {
    *p_of_find = 
	bdd_apply1_dont_add_roots(old_bddm, *p_of_find, bddm, &double_leaf_fn); 
    *q_of_find = 
	bdd_apply1_dont_add_roots(old_bddm, *q_of_find, bddm, &double_leaf_fn);
  }
  


  /*perform user supplied updates*/
  if (update_fn)
      (*update_fn)(&get_new_r);

  /*old_table now contains nodes whose mark field designates the
    new position of the node*/
  if (bddm->cache) {
    if (bddm->cache_erase_on_doubling) {
        bdd_kill_cache(bddm);
	bdd_make_cache(bddm, 2 * bddm->cache_size * CACHE_NUMBER_OF_BINS, 
		      2 * bddm->cache_overflow_increment * CACHE_NUMBER_OF_BINS);
      }
    else /*this is only a good idea when bddm is different from the  managers
	   the current apply operation is performed over*/
	double_cache(bddm, &get_new_r);
 }

  old_bddm->cache = (cache_record*) 0; /* old cache has been deallocated by now*/

  /*deallocated old table and old roots*/
  bdd_kill_manager(old_bddm);
}
