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
#include <string.h>
#include "bdd.h"
#include "bdd_internal.h"

unsigned bdd_apply1_dont_add_roots(bdd_manager *bddm_p, unsigned p, 
		    bdd_manager *bddm_r,
		    unsigned (*apply1_leaf_function)(unsigned value));

unsigned unsigned_log_ceiling(unsigned i) {	
unsigned j, k;
for (j = 0, k = 1; k < i ; j++, k = k << 1);
return(j);	    
}	

unsigned unsigned_exponential(unsigned i) {	
unsigned j;
for (j = 1; i ; i--, j = j << 1);
return(j);	    
}	


/* set out statistics data structures */
void mona_bdd_init(void) {
  struct stat_item *r;
  unsigned i, j;
  for (i = 0; i < BDD_STAT_INDEX_SIZE; i++) {
    stat_record[i].number_insertions = 0;
    stat_record[i].max_index = 0;
    for (j = 0; j < BDD_MAX_TABLE_INDEX ; j++) {  
      r = &stat_record[i].statistics[j];
      r->number_bddms = 0;
      r->number_double = 0;
      r->number_cache_collissions = 0;
      r->number_node_collissions = 0;
      r->number_node_link_followed = 0;
      r->number_insert_cache = 0;
      r->number_lookup_cache = 0;
      r->apply1_steps = 0;
      r->apply2_steps = 0;
    }
  }
}

bdd_manager *bdd_new_manager(unsigned table_size, 
			     unsigned table_overflow_increment){
  bdd_manager *new_bddm = 
      (bdd_manager*) mem_alloc((size_t)(sizeof (bdd_manager)));

  /*make table of BDD nodes*/
  new_bddm->table_log_size = unsigned_log_ceiling(table_size);

  new_bddm->table_next = BDD_NUMBER_OF_BINS; /*used for sequential operations*/
  /*we use 0 for indicating a miss in the result cache, so
    skip that position, but keep CPU cache alignment*/

  new_bddm->table_size  = unsigned_exponential(new_bddm->table_log_size);
  if (new_bddm->table_size < BDD_NUMBER_OF_BINS)
      new_bddm->table_size= BDD_NUMBER_OF_BINS;
  new_bddm->table_overflow_increment = 
      (table_overflow_increment < BDD_NUMBER_OF_BINS) ?
	  BDD_NUMBER_OF_BINS : table_overflow_increment;
  new_bddm->table_total_size = new_bddm->table_size 
      + BDD_NUMBER_OF_BINS + new_bddm->table_overflow_increment;

  if (new_bddm->table_total_size > BDD_MAX_TOTAL_TABLE_SIZE) {
    printf("\nBDD too large (>%d nodes)\n", BDD_MAX_TOTAL_TABLE_SIZE);
    abort();
  }

  new_bddm->node_table = (bdd_record*) 
    mem_alloc((size_t)
	      new_bddm->table_total_size
	      * (sizeof (bdd_record)));
  new_bddm->table_mask = new_bddm->table_size 
      - BDD_NUMBER_OF_BINS; /*to zero out the last log BDD_NUMBER_OF_BINS*/

  new_bddm->table_elements = 0;

  /*prepare hashed access*/
  new_bddm->table_overflow = new_bddm->table_size + BDD_NUMBER_OF_BINS;
  new_bddm->table_double_trigger = (4 * new_bddm->table_size) / 4;
  mem_zero(&new_bddm->node_table[BDD_NUMBER_OF_BINS], (size_t)
	   new_bddm->table_size * (sizeof (bdd_record)));
  new_bddm->cache_erase_on_doubling = TRUE;
  

  MAKE_SEQUENTIAL_LIST(new_bddm->roots, unsigned, 1024);


  /*indicate that there is no result cache*/
  new_bddm->cache = (cache_record*)0;
 
  /*prepare statistics*/
  new_bddm->number_double = 0;
  new_bddm->number_node_collissions = 0;
  new_bddm->number_node_link_followed = 0;
  new_bddm->number_cache_collissions = 0;
  new_bddm->number_cache_link_followed = 0;
  new_bddm->number_insert_cache = 0;
  new_bddm->number_lookup_cache = 0;
  new_bddm->apply1_steps = 0;
  new_bddm->apply2_steps = 0;
  new_bddm->call_steps = 0;
  return(new_bddm);
}

void bdd_make_cache(bdd_manager *bddm, unsigned size, unsigned overflow_increment) {
  unsigned overflow  = overflow_increment / CACHE_NUMBER_OF_BINS;
  bddm->cache_size = unsigned_exponential(unsigned_log_ceiling(size) -
					  CACHE_LOG_NUMBER_OF_BINS);
  bddm->cache_total_size = bddm->cache_size + overflow;
  bddm->cache_overflow_increment = overflow;
  bddm->cache_overflow = bddm->cache_size; 
  bddm->cache_mask = bddm->cache_size - 1;
  bddm->cache = (cache_record*)
      mem_alloc((size_t)(bddm->cache_total_size * (sizeof (cache_record))));
  {/*initialize hashed area*/
  mem_zero(bddm->cache, (size_t)(bddm->cache_size * (sizeof (cache_record))));
  }  
}

void bdd_kill_cache(bdd_manager *bddm) { 
  if (bddm->cache) {
    mem_free(bddm->cache);
  }
  bddm->cache = (cache_record*) 0;
} 

void bdd_update_statistics(bdd_manager *bddm, unsigned stat_index) {
  struct stat_item *r;
  stat_record[stat_index].number_insertions++;
  if (stat_record[stat_index].max_index < bddm->table_log_size) {
    stat_record[stat_index].max_index = bddm->table_log_size;
  }
  r = &stat_record[stat_index].statistics[bddm->table_log_size];
  r->number_bddms++;
  r->number_double += bddm->number_double;
  r->number_cache_collissions += bddm->number_cache_collissions;
  r->number_cache_link_followed += bddm->number_cache_link_followed;
  r->number_node_collissions += bddm->number_node_collissions;
  r->number_node_link_followed += bddm->number_node_link_followed;
  r->number_lookup_cache += bddm->number_lookup_cache;
  r->number_insert_cache += bddm->number_insert_cache;
  r->apply1_steps += bddm->apply1_steps;
  r->apply2_steps += bddm->apply2_steps;
}

void bdd_kill_manager(bdd_manager *bddm) { 
  mem_free(bddm->node_table);
  FREE_SEQUENTIAL_LIST(bddm->roots);
  if (bddm->cache) {
    mem_free(bddm->cache);
  }
  mem_free(bddm);
} 


void bdd_print_statistics(unsigned stat_index, char info[]) {
  const char title[]         = "%4s %6s %6s %8s %8s %8s %8s %8s %8s %8s %8s\n";
  const char form[]          = "%4i %6i %6i %8i %8i %8i %8i %8i %8i %8i %8i\n";
  const char total_form[]    = "%4s %6i %6i %8i %8i %8i %8i %8i %8i %8i %8i\n";

  unsigned i;
  struct stat_item *r;
  struct stat_item total = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

  printf("Statistics: %s.  Collected: %i\n", 
	 info, stat_record[stat_index].number_insertions);
  printf(title, "size", "bddms", "double", "app1", "app2", 
	 "node coll", "node link", "cach look", "cach ins", "cach coll", "cach link");
  for (i = 0; i <= stat_record[stat_index].max_index; i++) {  
    r = &stat_record[stat_index].statistics[i];
    printf(form, 
	   i, r->number_bddms, r->number_double,
           r->apply1_steps, r->apply2_steps, 
	   r->number_node_collissions, 
	   r->number_node_link_followed, 
	   r->number_lookup_cache,
	   r->number_insert_cache,
	   r->number_cache_collissions,
	   r->number_cache_link_followed);
    total.number_bddms += r->number_bddms;
    total.number_double += r->number_double;
    total.number_node_collissions += r->number_node_collissions;
    total.number_node_link_followed += r->number_node_link_followed;
    total.number_lookup_cache += r->number_lookup_cache;
    total.number_insert_cache += r->number_insert_cache;
    total.number_cache_collissions += r->number_cache_collissions;
    total.number_cache_link_followed += r->number_cache_link_followed;
    total.apply1_steps += r->apply1_steps;
    total.apply2_steps += r->apply2_steps;
 }   
 printf(total_form, 
	   "tot", total.number_bddms, total.number_double,
	   total.apply1_steps, total.apply2_steps, 
	   total.number_node_collissions, 
	   total.number_node_link_followed, 
	   total.number_lookup_cache,
	   total.number_insert_cache,
	   total.number_cache_collissions,
	   total.number_cache_link_followed);
}

unsigned mona_bdd_size(bdd_manager *bddm) {
return (bddm->table_elements);}
    
unsigned *bdd_roots(bdd_manager *bddm) {
  return (SEQUENTIAL_LIST(bddm->roots));  
}


void bdd_add_root(bdd_manager *bddm, unsigned p) {
  PUSH_SEQUENTIAL_LIST(bddm->roots, unsigned, p);
}

unsigned bdd_roots_length(bdd_manager *bddm) {
  return (LENGTH_SEQUENTIAL_LIST(bddm->roots));  
}

unsigned bdd_mark(bdd_manager *bddm, bdd_ptr p) {
  return bddm->node_table[p].mark; 
}

void bdd_set_mark(bdd_manager *bddm, bdd_ptr p, unsigned mark) {
  bddm->node_table[p].mark = mark; 
}
