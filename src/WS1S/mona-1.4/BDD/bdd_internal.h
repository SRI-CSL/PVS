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

#ifndef _BDD_INTERNAL
#define _BDD_INTERNAL

#include "../Mem/mem.h"

/*ARCHITECTURE DEPENDENT CONSTANTS*/
/*memory allocation should take place in chunks the size of, or divisible by,
this constant*/
#define PROCESSOR_CACHE_LINE_SIZE 32

/*the number of bins per bucket; hash function indicates the
  address of the 0th bin*/
#define BDD_LOG_NUMBER_OF_BINS 1
#define BDD_NUMBER_OF_BINS 2

#define CACHE_LOG_NUMBER_OF_BINS 1
#define CACHE_NUMBER_OF_BINS 2

/*logarithm of the maximal number of BDD nodes*/
#define BDD_MAX_TABLE_INDEX  24 


#define HASH2(p, q, mask) \
((((((unsigned)p) * 46349) + (unsigned)q) *  67108859) & mask)

#define HASH3(p, q, r, mask) \
((((((((unsigned)p)  *  46349) + (unsigned)q)  *  67108859)  + (unsigned)r) \
  * 10007) & mask)

/*CACHE DATA TYPES AND ELEMENTARY OPERATIONS*/

/* a cache record contains two bins */

struct cache_record_
{unsigned p0, q0, res0, p1, q1, res1;
 unsigned next;
 unsigned align; /*to make record fill 32 bytes*/
};

#define CACHE_UNUSED ((unsigned) 0)
#define CACHE_USED ((unsigned) 1)

#define CACHE_INITIALIZE_RECORD(cache_rec) \
cache_rec.p0 = cache_rec.p1 = CACHE_UNUSED; \
cache_rec.next = 0;
	
#define CACHE_LOOKUP_BIN0(cache_rec, p, q) \
(((p == cache_rec.p0) && (q == cache_rec.q0))? cache_rec.res0 : 0)

#define CACHE_LOOKUP_BIN1(cache_rec, p, q) \
(((p == cache_rec.p1) && (q == cache_rec.q1))? cache_rec.res1 : 0)

#define CACHE_LOOKUP_BINS(cache_rec, p, q, res) \
((res = CACHE_LOOKUP_BIN0(cache_rec, p, q)) ? \
    res : (res = CACHE_LOOKUP_BIN1(cache_rec, p, q))) \
 

#ifdef _BDD_STAT_  
 
#define CACHE_LOOKUP_RECORD(bddm, cache_rec, p, q, res, temp) \
{if (!(CACHE_LOOKUP_BINS(cache_rec, p, q, res))) \
    for	(temp = cache_rec.next; temp; temp = bddm->cache[temp].next) { \
       bddm->number_cache_link_followed++; \
       if (CACHE_LOOKUP_BINS(bddm->cache[temp], p, q, res))   \
	    break;} \
}\

#else

#define CACHE_LOOKUP_RECORD(bddm, cache_rec, p, q, res, temp) \
{if (!(CACHE_LOOKUP_BINS(cache_rec, p, q, res))) \
    for	(temp = cache_rec.next; temp; temp = bddm->cache[temp].next) { \
       if (CACHE_LOOKUP_BINS(bddm->cache[temp], p, q, res))   \
	    break;} \
}\

#endif

#define CACHE_FULL_BIN0(cache_rec) \
(cache_rec.p0 != CACHE_UNUSED)\

#define CACHE_FULL_BIN1(cache_rec) \
(cache_rec.p1 != CACHE_UNUSED)\
   
#define CACHE_CLEAR_BIN0(cache_rec) \
cache_rec.p0 = CACHE_UNUSED; \

#define CACHE_CLEAR_BIN1(cache_rec) \
cache_rec.p1 = CACHE_UNUSED; \

#define CACHE_STORE_BIN0(cache_rec, p, q, res) \
cache_rec.p0 = p; \
cache_rec.q0 = q; \
cache_rec.res0 = res; \

#define CACHE_STORE_BIN1(cache_rec, p, q, res) \
cache_rec.p1 = p; \
cache_rec.q1 = q; \
cache_rec.res1 = res; \

#define CACHE_LOAD_BIN0(cache_rec, p, q, res) \
p = cache_rec.p0; \
q = cache_rec.q0; \
res = cache_rec.res0; \

#define CACHE_LOAD_BIN1(cache_rec, p, q, res) \
p = cache_rec.p1; \
q = cache_rec.q1; \
res = cache_rec.res1; \

#define CACHE_NEW_RECORD(bddm, temp) \
if (bddm->cache_overflow == bddm->cache_total_size) \
    bddm->cache = mem_resize(bddm->cache,  \
				   (size_t) (sizeof (cache_record))\
				   * (bddm->cache_total_size \
				      += bddm->cache_overflow_increment)); \
temp=bddm->cache_overflow++; \
CACHE_INITIALIZE_RECORD(bddm->cache[temp]);\


/* STATISTICS */

struct stat_item { 
  unsigned number_bddms;
  unsigned number_double;
  unsigned number_node_collissions;
  unsigned number_node_link_followed;
  unsigned number_cache_collissions;
  unsigned number_cache_link_followed;
  unsigned number_lookup_cache;
  unsigned number_insert_cache;
  unsigned apply1_steps;
  unsigned apply2_steps;
};

struct stat_record_ {
  unsigned max_index;
  unsigned number_insertions;
  struct stat_item statistics[BDD_MAX_TABLE_INDEX];
};

extern struct stat_record_ stat_record[];
extern boolean table_has_been_doubled;

inline unsigned lookup_cache(bdd_manager *bddm, unsigned *h,
			     unsigned p, unsigned q);

inline void insert_cache(bdd_manager *bddm, unsigned h, 
			 unsigned p, unsigned q, unsigned res);

void double_cache( bdd_manager *bddm,
		  unsigned (*result_fn)(unsigned r));

void double_table_sequential(bdd_manager *bddm);

void double_table_and_cache_hashed(bdd_manager *bddm,
				   unsigned* some_roots,
				   void (*update_fn)(unsigned (*new_place)
						     (unsigned node)),
				   unsigned *p_of_find, unsigned *q_of_find,
				   boolean rehash_p_and_q);

unsigned bdd_apply1_dont_add_roots(bdd_manager *bddm_p, 
				   unsigned p, 
				   bdd_manager *bddm_r,
				   unsigned (*apply1_leaf_function)
				   (unsigned value));


#endif
