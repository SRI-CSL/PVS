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

/*CACHE DOUBLING stuff*/

void copy_cache_record_and_overflow(bdd_manager *bddm, 
				    cache_record *old_cache, 
				    unsigned  i,
				    unsigned (*result_fn)(unsigned r)) {
  unsigned p, q, r, h;
  if (CACHE_FULL_BIN0(old_cache[i])) {
    CACHE_LOAD_BIN0(old_cache[i], p, q, r);
    h = HASH2(p, q, bddm->cache_mask);
    insert_cache(bddm, h, p, q, result_fn(r));
    if (CACHE_FULL_BIN1(old_cache[i])) {
      CACHE_LOAD_BIN1(old_cache[i], p, q, r);
      h = HASH2(p, q, bddm->cache_mask);
      insert_cache(bddm, h, p, q, result_fn(r));
    }
  }
  if (old_cache[i].next != 0) {
    copy_cache_record_and_overflow(bddm, old_cache,
				   old_cache[i].next, result_fn);
  }	
}	

void double_cache(bdd_manager *bddm,
		  unsigned (*result_fn)(unsigned r)) {
  cache_record *old_cache = bddm->cache;
  unsigned i;
  unsigned old_size = bddm->cache_size;
  bddm->cache_size *= 2;
  bddm->cache_overflow_increment *= 2;
  bddm->cache_total_size = bddm->cache_size + bddm->cache_overflow_increment;
  bddm->cache_overflow = bddm->cache_size;
  bddm->cache = (cache_record*)
      mem_alloc((size_t) bddm->cache_total_size * (sizeof (cache_record)));

  /*initialize hashed area*/
  mem_zero(bddm->cache, (size_t)(bddm->cache_size * (sizeof (cache_record))));

  bddm->cache_mask = bddm->cache_size - 1;

/*  printf("Doubling cache to: Cache %u Total %u\n", bddm->cache_size, bddm->cache_total_size); */

  /*now rehash in an entirely sequential fashion (except for overflow)*/
  for (i = 0; i < old_size ; i++) {
    CACHE_INITIALIZE_RECORD(bddm->cache[i]);
    CACHE_INITIALIZE_RECORD(bddm->cache[i + old_size]);
    /*since the hash function is the same for the new cache except
      that an extra bit in position (log old_size) is added*/
    copy_cache_record_and_overflow(bddm, old_cache, i, result_fn);
  }

  mem_free(old_cache);
}
