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

extern int memlimit;

struct stat_record_ stat_record[BDD_STAT_INDEX_SIZE];

boolean table_has_been_doubled;


/* if something is cached for (p, q), then return it; otherwise, return
   0.  In any case, set *h to the hash value calculated) */

inline unsigned lookup_cache(bdd_manager *bddm, unsigned *h,
			     unsigned p, unsigned q)  {
  unsigned temp, res;
   cache_record *cache_ptr;
#ifdef _BDD_STAT_
    bddm->number_lookup_cache++;
#endif
  *h = HASH2(p, q, bddm->cache_mask); 
  cache_ptr = &bddm->cache[*h];
  CACHE_LOOKUP_RECORD(bddm, (*cache_ptr), p, q, res, temp);
  return (res);
}

/* insert in cache, while calculating cache value.  Note: bddm->cache
   may change during this operation */
inline void insert_cache(bdd_manager *bddm, unsigned h, 
			 unsigned p, unsigned q, unsigned res){
  unsigned temp;
  unsigned p_, q_, r_;

 cache_record *cache_ptr = &bddm->cache[h];

#ifdef _BDD_STAT_
    bddm->number_insert_cache++;
#endif

  if (!CACHE_FULL_BIN0((*cache_ptr)))  
      {CACHE_STORE_BIN0((*cache_ptr), p, q, res);} 
  else if (!CACHE_FULL_BIN1((*cache_ptr)))  
      {CACHE_STORE_BIN1((*cache_ptr), p, q, res);} 
  else {	 
#ifdef _BDD_STAT_
    bddm->number_cache_collissions++;
#endif
    CACHE_NEW_RECORD(bddm, temp); 
    cache_ptr = &bddm->cache[h]; /*since preceding creation of record
			           may have changed bddm->cache*/
    CACHE_LOAD_BIN0((*cache_ptr), p_, q_, r_);
    CACHE_STORE_BIN0(bddm->cache[temp], p_, q_, r_); 
    CACHE_LOAD_BIN1((*cache_ptr), p_, q_, r_);
    CACHE_STORE_BIN1(bddm->cache[temp], p_, q_, r_); 
    CACHE_STORE_BIN0((*cache_ptr), p, q, res); 
    CACHE_CLEAR_BIN1((*cache_ptr));
    bddm->cache[temp].next = (*cache_ptr).next; 
    cache_ptr->next = temp;
  }
}




unsigned bdd_ifindex(bdd_manager *bddm, unsigned p) {
  unsigned index;
  LOAD_index(&bddm->node_table[p], index);
  return (index);
}

unsigned bdd_then(bdd_manager *bddm, unsigned p) {
  unsigned l, r, index;
  LOAD_lri(&bddm->node_table[p], l, r, index);
  return (r);
}

unsigned bdd_else(bdd_manager *bddm, unsigned p) {
  unsigned l, r, index;
  LOAD_lri(&bddm->node_table[p], l, r, index);
  return (l);
}

unsigned bdd_is_leaf(bdd_manager *bddm, unsigned p) {
  unsigned index;
  LOAD_index(&bddm->node_table[p], index);
   return  (index == BDD_LEAF_INDEX);
}

unsigned bdd_leaf_value(bdd_manager *bddm, unsigned p) {
  unsigned l, r, index;
  LOAD_lri(&bddm->node_table[p], l, r, index);
  return (l);
}


unsigned same_r(unsigned r) {
  return (r);
}

inline bdd_ptr bdd_get_free_node_sequential(bdd_manager *bddm) {
  bddm->table_elements++;
  if (bddm->table_next < bddm->table_total_size) {
    return (bddm->table_next++);    
  }   
  else {
    double_table_sequential(bddm);
    if (bddm->cache) {
      double_cache(bddm, &same_r);    
    }
    return (bddm->table_next++);
  }
}

bdd_ptr bdd_find_node_sequential(bdd_manager *bddm,
				  unsigned l, unsigned r, unsigned i) {
  unsigned res =  bdd_get_free_node_sequential(bddm);
  invariant(i <= BDD_MAX_INDEX);
  STR_lri(&bddm->node_table[res], l, r, i);
  return (res);
}

inline unsigned bdd_find_leaf_sequential(bdd_manager *bddm, unsigned val) {
  unsigned res = bdd_get_free_node_sequential(bddm);
  STR_lri(&bddm->node_table[res], val, BDD_USED, BDD_LEAF_INDEX);
  return (res);
}


inline unsigned bdd_find_node_hashed(bdd_manager *bddm,
			      unsigned l, unsigned r, unsigned indx,
			      unsigned *some_roots,
			      void (*update_fn)(unsigned 
						(*new_place)(unsigned node))) {
  unsigned h;
  bdd_record *ptr;
  unsigned i0, i1;
  unsigned i;
  unsigned temp;

  table_has_been_doubled = FALSE;

start: 
 
  h = HASH3(l, r, indx, bddm->table_mask) +  BDD_NUMBER_OF_BINS;
  ptr = &bddm->node_table[h];
  TWO_UNS_STR_lri(i0, i1, l, r, indx);
  
  /*look in primary bucket indicated by hash function*/
  for (i = BDD_NUMBER_OF_BINS; i--; ) {
    if (LOAD_r(ptr + i) == BDD_UNUSED) {
      goto insert_in_existing;
    } else {
      if (i0 == (ptr + i)->lri[0] && i1 == (ptr + i)->lri[1]) {      
	return (h + i);
      }
    }
  }
  
  /*now look in overflow area*/
  for (temp = ptr->next; temp; temp = bddm->node_table[temp].next) {
#ifdef _BDD_STAT_
    bddm->number_node_link_followed++;
#endif
    ptr = &bddm->node_table[temp];
    for (i = BDD_NUMBER_OF_BINS; i--; ) {
      if (LOAD_r(ptr + i) == BDD_UNUSED) {  
	goto insert_in_existing;
      }
      else if (i0 == (ptr + i)->lri[0] && i1 == (ptr + i)->lri[1]) { 
	return (temp + i);
      }
    }
  }

  goto insert_in_new_bucket;


  /*we didn't find the node, but found an available bin*/
insert_in_existing:

  bddm->table_elements++;
  
  (ptr + i)->lri[0] = i0;
  (ptr + i)->lri[1] = i1;
  (ptr + i)->mark = 0;
  return ((ptr - bddm->node_table) + i);

  /*no room; create a new bucket in overflow area*/
insert_in_new_bucket:


#ifdef _BDD_STAT_
  bddm->number_node_collissions++;
#endif
   
  /*but first check that table is not too full to warrant doubling*/
  if (bddm->table_elements > bddm->table_double_trigger) {
/*    printf("double_table_and_cache_hashed called:\n"); */
    /*only change  l and r if node is not a leaf*/
    double_table_and_cache_hashed(bddm, some_roots, update_fn,
				  &l, &r, (indx != BDD_LEAF_INDEX));
    /*now nodes l and r are possibly to be found in new places, and we
      should use quite a different primary bucket, so...*/
    table_has_been_doubled = TRUE;
    goto start;
  }
 

  /*and check that overflow area is big enough; be careful with ptr,
    which should be pointing to same indexed position even if
    node_table is moved */
  if (bddm->table_overflow + BDD_NUMBER_OF_BINS > bddm->table_total_size) {
    unsigned ptr_as_index = ptr - bddm->node_table;
    bddm->table_total_size += bddm->table_overflow_increment;
    bddm->node_table = mem_resize(bddm->node_table, 
					(size_t) (sizeof (bdd_record))
					* bddm->table_total_size);
    ptr = bddm->node_table + ptr_as_index;
  }
  /* now do the insertion */

  bddm->table_elements++;

  ptr->next = bddm->table_overflow;

  ptr = &bddm->node_table[bddm->table_overflow];
  (ptr + (BDD_NUMBER_OF_BINS - 1))->lri[0] = i0;
  (ptr + (BDD_NUMBER_OF_BINS - 1))->lri[1] = i1;
  (ptr + (BDD_NUMBER_OF_BINS - 1))->mark = 0;
  ptr->next = 0; /*unnecessary to initialize other next fields in
		   same bucket as we only use the next field of the 0th*/


  /*but mark other bins as unused*/
  for (i = BDD_NUMBER_OF_BINS - 1; i--; ) {
    (ptr + i)->lri[1] = BDD_UNUSED; (ptr + i)->lri[0] = BDD_UNUSED;
  }
  /*advance table_overflow to next available bucket*/
  return ((bddm->table_overflow += BDD_NUMBER_OF_BINS) - 1);
}


inline bdd_ptr bdd_find_node_hashed_add_root(bdd_manager *bddm,
				             bdd_ptr l, bdd_ptr r, unsigned indx) {
  bdd_ptr p;
  invariant(indx <= BDD_MAX_INDEX);
  p = bdd_find_node_hashed(bddm, l, r, indx, BDD_ROOTS(bddm), NULL);
  BDD_ADD_ROOT(bddm, p);
  return (p);
}

inline bdd_handle bdd_handle_find_node_hashed_add_root(bdd_manager *bddm,
				              bdd_ptr l, bdd_ptr r, unsigned indx) {
  bdd_ptr p;
  invariant(indx <= BDD_MAX_INDEX);
  p = bdd_find_node_hashed(bddm, l, r, indx, BDD_ROOTS(bddm), NULL);
  BDD_ADD_ROOT(bddm, p);
  return (BDD_LAST_HANDLE(bddm));
}


inline unsigned bdd_find_leaf_hashed(bdd_manager *bddm, unsigned val,
			      void *some_roots,
			      void (*update_fn)(unsigned (*new_place)(unsigned node))) {
 return (bdd_find_node_hashed(bddm, val, BDD_USED, BDD_LEAF_INDEX, some_roots, update_fn));
}

inline bdd_ptr bdd_find_leaf_hashed_add_root(bdd_manager *bddm,
					     unsigned val) {
  bdd_ptr p = bdd_find_leaf_hashed(bddm, val, BDD_ROOTS(bddm), NULL);
  BDD_ADD_ROOT(bddm, p);
  return (p);
}

inline bdd_handle bdd_handle_find_leaf_hashed_add_root(bdd_manager *bddm,
 						       unsigned val) {
  bdd_ptr p = bdd_find_leaf_hashed(bddm, val, BDD_ROOTS(bddm), NULL);
  BDD_ADD_ROOT(bddm, p);
  return (BDD_LAST_HANDLE(bddm));
}

/*APPLY ROUTINES*/
#define DECLARE_LOCAL(record) \
struct local_##record {\
  unsigned a_size;\
  record *act_stack;\
  record *a;\
  record *a_last;\
  bdd_manager *bddm_p, *bddm_q, *bddm_r;\
}; \
unsigned local_##record##_in_use = 0; \
struct local_##record *local_##record##_primary = NULL

#define DECLARE_POINTER_TO_LOCAL(record, pointer) \
struct local_##record *pointer

/* we keep a distinguished local record, kept through
local_##record##_primary; usually, we can just use that one without
wasting time getting memory */

#define NEW_LOCAL(record, local, bddm_p, bddm_q, bddm_r) \
record *a; \
if (local_##record##_primary && (local_##record##_in_use == 0)) \
   local = local_##record##_primary; /* use the current one */ \
else { \
  local = (struct local_##record *) \
     mem_alloc((size_t)(sizeof (struct local_##record))); \
  local->a_size = 1024; \
  local->act_stack = (record*) \
      mem_alloc((size_t)(sizeof (record)) * local->a_size); \
  local->a_last = &(local->act_stack[local->a_size - 1]); \
  if (!local_##record##_primary) \
    local_##record##_primary = local; \
}; \
local->a = local->act_stack; \
a = local->a; \
local->bddm_p = bddm_p; \
local->bddm_q = bddm_q; \
local->bddm_r = bddm_r; \
local_##record##_in_use++

#define INCREMENT_LOCAL(local) \
if (local->a == local->a_last) { \
    unsigned size = ((local->a_last - local->act_stack) + 1) * 2; \
    unsigned i = local->a_last - local->act_stack; \
    local->act_stack = mem_resize(local->act_stack,  \
				 (size_t)(sizeof *(local->act_stack))  \
				  * size); \
    local->a_last =  &(local->act_stack)[size - 1];  \
    local->a = &(local->act_stack)[i + 1]; \
  } else \
      local->a++; \
a = local->a 

#define DECREMENT_LOCAL(local) \
local->a--; \
a = local->a 

#define BOTTOM_LOCAL(local) \
local->a == local->act_stack \

#define BEGINNING_LOCAL(local) \
local->act_stack \

#define FREE_LOCAL(record,local) \
if (local_##record##_in_use > 1) { \
  mem_free(local->act_stack); \
  mem_free(local); }; \
local_##record##_in_use--



#define DECLARE_A(record) \
  unsigned a_size = 1024; \
  record *act_stack = (record*) \
      mem_alloc((size_t)(sizeof (record)) * a_size); \
   record *a  = act_stack; \
  record *a_last = &act_stack[a_size - 1] \

#define INCREMENT_A \
  if (a == a_last) { \
    unsigned size =  ((a_last - act_stack) + 1) * 2; \
    unsigned i = a_last - act_stack; \
    act_stack = mem_resize(act_stack,  \
				  (size_t)(sizeof *act_stack)  \
				  * size); \
    a_last =  &(act_stack)[size - 1];  \
    a = &(act_stack)[i + 1]; \
  } else \
      a++; \

#define DECREMENT_A \
a-- \

#define BOTTOM_A \
a == act_stack \

#define BEGINNING_A \
act_stack

#define FREE_A \
mem_free(act_stack) \

    
const unsigned going_left = BDD_UNDEF;
const unsigned hash_value_invalid = (unsigned) (-1);
    

void bdd_prepare_apply1(bdd_manager *bddm) {
  bdd_record *p; 
  for (p = &bddm->node_table[BDD_NUMBER_OF_BINS]; 
       p < &bddm->node_table[bddm->table_total_size];  
       p++) {
    p->mark = 0;
  }
}

typedef struct {
  int index;
  unsigned p;
  unsigned second;
} activation_record_apply1;

/* declare the type of activation frames for apply1 */
DECLARE_LOCAL(activation_record_apply1);

/* apply1_ptr points to the current activation frame for
   an apply1 operation */
DECLARE_POINTER_TO_LOCAL(activation_record_apply1, apply1_ptr);


void update_activation_stack(unsigned (*new_place)(unsigned node)) {
  activation_record_apply1 *a_;
  if (apply1_ptr->bddm_p == apply1_ptr->bddm_r) {
    for (a_ = apply1_ptr->act_stack; a_ <= apply1_ptr->a ; a_++) {   
      a_->p = new_place(a_->p);
      /* a corresponds to a leaf, and a_->second is only well_defined 

       * for internal nodes */
      if (apply1_ptr->a != a_) 
	a_->second = new_place(a_->second);
    }	    
  }
  /* all mark fields are invalid at this point */
  bdd_prepare_apply1(apply1_ptr->bddm_p);
}


unsigned bdd_apply1_internal(bdd_manager *bddm_p, unsigned p, 
		    bdd_manager *bddm_r,
		    unsigned (*apply1_leaf_function)(unsigned value),
		    boolean add_roots) {
  
  unsigned res;
  bdd_record *node_ptr;
  unsigned tmp1, tmpp;

  bdd_record *p_table;

  p_table = bddm_p->node_table; 
  node_ptr = &p_table[p];
  
  if ((res = node_ptr->mark) != 0) {

#ifdef _BDD_STAT_
    bddm_r->apply1_steps++;
#endif
    if (add_roots) {
      PUSH_SEQUENTIAL_LIST(bddm_r->roots, unsigned, res);
    }   
    return res; 
  }
  else {
  
    DECLARE_POINTER_TO_LOCAL(activation_record_apply1, local);
    DECLARE_POINTER_TO_LOCAL(activation_record_apply1, old_apply1_ptr);
    /* bdd_apply1 restores the local data; this is necessary,
       for example when the table is doubled during an bdd_apply1
       (which entails a call of bdd_apply1 to copy the table into
       a fresh one) */ 

    DECLARE_SEQUENTIAL_LIST(intermediate, unsigned)
    
    NEW_LOCAL(activation_record_apply1, local,
	      bddm_p, bddm_p, bddm_r); /* second last argument is dummy */

    MAKE_SEQUENTIAL_LIST(intermediate, unsigned, 1024);

    old_apply1_ptr = apply1_ptr;
    apply1_ptr = local;

  start:
    node_ptr = &p_table[p];
  
#ifdef _BDD_STAT_
    bddm_r->apply1_steps++;
#endif

    PUSH_SEQUENTIAL_LIST(intermediate, unsigned, going_left);
    
    if ((res = node_ptr->mark) != 0) 
      goto finish;
    else {
      LOAD_index(node_ptr,a->index); /*get index*/ /*CACHE MISS STALL*/
    
      if (a->index == BDD_LEAF_INDEX) {
	/*we are at leaf*/
	LOAD_lr(node_ptr, tmpp, tmp1);
	res = bdd_find_leaf_hashed(bddm_r, apply1_leaf_function(tmpp),
				   SEQUENTIAL_LIST(intermediate),
				   /*  (void*)0); */
				   &update_activation_stack); 
        p_table = bddm_p->node_table; 
	node_ptr = &p_table[p];
  	node_ptr->mark = res; 

	goto finish;}
      else { 
	a->p = p;
	/*descend left from p, then right*/ 
	LOAD_lr(node_ptr, p, a->second);
	INCREMENT_LOCAL(local); /*prepare recursion*/
	/*now do the apply for the left successor*/
	goto start;}
    
    second:
      /*we are now done with apply along the left*/
      /*now do the apply along the right*/
      STORE_TOP_SEQUENTIAL_LIST(intermediate, res);
      p = a->second;
      INCREMENT_LOCAL(local);
      goto start;
    
      /*we are done with both left and right apply's*/
    third:
      if (TOP_SEQUENTIAL_LIST(intermediate) == res) {
	/* then res is the right answer, because of path 
	   compression */
      } else {
	res = bdd_find_node_hashed(bddm_r, TOP_SEQUENTIAL_LIST(intermediate), 
				   res, a->index, 
				   SEQUENTIAL_LIST(intermediate), 
				   &update_activation_stack);       
       
      }
      p_table = bddm_p->node_table; 
      p_table[a->p].mark = res; /*CACHE MISS STALL*/
    
    finish:
      if (BOTTOM_LOCAL(local)) {
	FREE_LOCAL(activation_record_apply1,local);
	FREE_SEQUENTIAL_LIST(intermediate);
	if (add_roots) {
	  PUSH_SEQUENTIAL_LIST(bddm_r->roots, unsigned, res);
	}
	apply1_ptr = old_apply1_ptr;
	return (res);
      }
      else {
	DECREMENT_LOCAL(local);
	PEEL_SEQUENTIAL_LIST(intermediate, unsigned);
	if (TOP_SEQUENTIAL_LIST(intermediate) == going_left)
	  goto second; 
	else
	  goto third;
      }
    }
  }
}

unsigned bdd_apply1(bdd_manager *bddm_p, unsigned p, 
		    bdd_manager *bddm_r,
		    unsigned (*apply1_leaf_function)(unsigned value)) {
   return (bdd_apply1_internal(bddm_p, p, bddm_r,apply1_leaf_function, TRUE));
}
  
unsigned bdd_apply1_dont_add_roots(bdd_manager *bddm_p, unsigned p, 
		    bdd_manager *bddm_r,
		    unsigned (*apply1_leaf_function)(unsigned value)) {
  return (bdd_apply1_internal(bddm_p, p, bddm_r,apply1_leaf_function, FALSE));
}



typedef struct {
  int index;
  unsigned p, q, h;
  unsigned second_p, second_q;
} activation_record_apply2_hashed;


DECLARE_LOCAL(activation_record_apply2_hashed);

/* apply2_ptr contains pointer to current activation frame
   of bdd_apply2_hashed */

DECLARE_POINTER_TO_LOCAL(activation_record_apply2_hashed, apply2_ptr);

void update_activation_stack_apply2_hashed
(unsigned (*new_place)(unsigned node)) {
  activation_record_apply2_hashed *a_;
  if (apply2_ptr->bddm_p == apply2_ptr->bddm_r) {
    for (a_ = apply2_ptr->act_stack; a_ <= apply2_ptr->a ; a_++) {   
      a_->p = new_place(a_->p);
      a_->h = hash_value_invalid;
      /* a correspond to a leaf, and a_->second_p is only well_defined 
       * for internal nodes */
      if (apply2_ptr->a!= a_) 
	a_->second_p = new_place(a_->second_p);
    }
    if (apply2_ptr->bddm_q == apply2_ptr->bddm_r) {
      for (a_ =  apply2_ptr->act_stack; a_ <= apply2_ptr->a; a_++) {   
	a_->q = new_place(a_->q);
	a_->h = hash_value_invalid;
	/* a correspond to a leaf, and a_->second_q is only well_defined 
	 * for internal nodes */
	if (apply2_ptr->a != a_)
	  a_->second_q = new_place(a_->second_q);	
      }	
    }
  }
}

unsigned bdd_apply2_hashed(bdd_manager *bddm_p, unsigned p, 
			   bdd_manager *bddm_q, unsigned q,
			    bdd_manager *bddm_r,
			   unsigned (*apply2_leaf_function)
			   (unsigned p_value, unsigned q_value)) {
  unsigned p_i, q_i;
  unsigned res;
  bdd_record *node_ptr;
  unsigned tmp1, tmpp, tmpq;
  unsigned h;
  bdd_record *p_table, *q_table;

  res = lookup_cache(bddm_r, &h, p, q); /*CACHE MISS STALL*/

  if (res != 0) {
  
#ifdef _BDD_STAT_
    bddm_r->apply2_steps++;
#endif
  
    PUSH_SEQUENTIAL_LIST(bddm_r->roots, unsigned, res);
    return res;
  }
  else {
 
    DECLARE_POINTER_TO_LOCAL(activation_record_apply2_hashed, local);
    DECLARE_POINTER_TO_LOCAL(activation_record_apply2_hashed, old_apply2_ptr);

    DECLARE_SEQUENTIAL_LIST(intermediate, unsigned)

    NEW_LOCAL(activation_record_apply2_hashed, local,
		bddm_p, bddm_q, bddm_r);

    MAKE_SEQUENTIAL_LIST(intermediate, unsigned, 1024);

    old_apply2_ptr = apply2_ptr;
    apply2_ptr = local;

  start:
  
#ifdef _BDD_STAT_
    bddm_r->apply2_steps++;
#endif
  
    PUSH_SEQUENTIAL_LIST(intermediate, unsigned, going_left);
  
    res = lookup_cache(bddm_r, &h, p, q); /*CACHE MISS STALL*/

    if (res != 0) 
      goto finish;
    else {
      p_table = bddm_p->node_table; /* when we get here from second: or
				       third: bddm_p->node_table may
				       have changed as a result of a
				       resize operation */
      q_table = bddm_q->node_table;
  
      node_ptr = &p_table[p];
      LOAD_index(node_ptr,p_i); /*get indices*/ /*CACHE MISS STALL*/
      node_ptr = &q_table[q];
      LOAD_index(node_ptr,q_i);                 /*CACHE MISS STALL*/
      a->p = p; /*this for later insert_cache operation*/
      a->q = q;
      a->h = h;

      if ((p_i == BDD_LEAF_INDEX) && (q_i == BDD_LEAF_INDEX)){
	/*we are at leaves*/
	node_ptr = &p_table[p];
	LOAD_lr(node_ptr, tmpp, tmp1);
	node_ptr = &q_table[q];
	LOAD_lr(node_ptr, tmpq, tmp1);

	res = bdd_find_leaf_hashed(bddm_r, apply2_leaf_function(tmpp, tmpq),
				   SEQUENTIAL_LIST(intermediate),
				   &update_activation_stack_apply2_hashed);
	if (a->h == hash_value_invalid)
	  a->h = HASH2(a->p, a->q, bddm_r->cache_mask); 
	insert_cache(bddm_r, a->h, a->p, a->q, res); 
	/*don't use p, q since they may have changed after bdd_find_leaf_hashed*/
	goto finish;}
      else if (p_i == q_i){
	/*descend in both*/
	/*descend left from p, then right*/ 
	node_ptr = &p_table[p];
	LOAD_lr(node_ptr, p, a->second_p);
	/*descend left from q, then right*/ 
	node_ptr = &q_table[q];
	LOAD_lr(node_ptr, q, a->second_q);
      
	a->index = p_i; /*remember for upcoming store operation*/
	INCREMENT_LOCAL(local); /*prepare recursion*/
	/*now	 do the apply for the left successor*/
	goto start;}
      else if ((q_i == BDD_LEAF_INDEX) || (p_i < q_i)){
	/*descend in p (which is not terminal), remain in q*/
	/*descend left from p, then right*/ 
	node_ptr = &p_table[p];
	LOAD_lr(node_ptr, p, a->second_p);
	a->second_q = q;
	a->index = p_i;
	INCREMENT_LOCAL(local);
	goto start;}
      else  {
	/*remain in p, descend in q (which is not terminal)*/
	/*descend left from q, then right*/ 
	node_ptr = &q_table[q];
	LOAD_lr(node_ptr, q, a->second_q);
	a->second_p = p;
	a->index = q_i;
	INCREMENT_LOCAL(local);
	goto start;}
    
    second:
      /*we are now done with apply along the left*/
      /*now do the apply along the right*/
      p = a->second_p;
      q = a->second_q;
      STORE_TOP_SEQUENTIAL_LIST(intermediate, res);
      INCREMENT_LOCAL(local);
      goto start;
    
      /*we are done with both apply's*/
    third:
      if (TOP_SEQUENTIAL_LIST(intermediate) == res) {
	/*then res is the right answer*/
      } else
	res = bdd_find_node_hashed(bddm_r, TOP_SEQUENTIAL_LIST(intermediate), 
				   res, a->index,
				   SEQUENTIAL_LIST(intermediate),
				   &update_activation_stack_apply2_hashed);  /*CACHE MISS STALL*/
    
      if (a->h == hash_value_invalid)
	a->h = HASH2(a->p, a->q, bddm_r->cache_mask); 
      insert_cache(bddm_r, a->h, a->p, a->q, res); /*CACHE MISS STALL*/
    
    finish:
      if (BOTTOM_LOCAL(local)) {
	FREE_LOCAL(activation_record_apply2_hashed, local);	
	FREE_SEQUENTIAL_LIST(intermediate);
	PUSH_SEQUENTIAL_LIST(bddm_r->roots, unsigned, res);
	apply2_ptr = old_apply2_ptr;
	return res;
      }
      else {
	DECREMENT_LOCAL(local);
	PEEL_SEQUENTIAL_LIST(intermediate, unsigned);
	if (TOP_SEQUENTIAL_LIST(intermediate) == going_left)
	  goto second;
	else
	  goto third;
      }
    }
  }
}

typedef struct {
  int index;
  unsigned result_node, inter_m, second_p, second_q;
} activation_record_apply2_sequential;

unsigned bdd_apply2_sequential(bdd_manager *bddm_p, unsigned p, 
			   bdd_manager *bddm_q, unsigned q,
			    bdd_manager *bddm_r,
			   unsigned (*apply2_leaf_function)
			   (unsigned p_value, unsigned q_value)) {
  unsigned p_i, q_i;
  unsigned res;
   bdd_record *node_ptr;
  unsigned tmp1, tmpp, tmpq;
  unsigned h;
   bdd_record *p_table, *q_table;
  
 
  res = lookup_cache(bddm_r, &h, p, q); /*CACHE MISS STALL*/

  if (res != 0) {

#ifdef _BDD_STAT_
    bddm_r->apply1_steps++;
#endif
  
    PUSH_SEQUENTIAL_LIST(bddm_r->roots, unsigned, res);
    return res;
  }  
  else {
   DECLARE_A(activation_record_apply2_sequential);
 

 start:
 
#ifdef _BDD_STAT_
  bddm_r->apply2_steps++;
#endif
  
  res = lookup_cache(bddm_r, &h, p, q); /*CACHE MISS STALL*/

  if (res != 0) 
      goto finish;
  else {
  
    
    p_table = bddm_p->node_table;
    q_table = bddm_q->node_table;

    node_ptr = &p_table[p];
    LOAD_index(node_ptr,p_i); /*get indices*/ /*CACHE MISS STALL*/
    node_ptr = &q_table[q];
    LOAD_index(node_ptr,q_i);                 /*CACHE MISS STALL*/
    a->inter_m = 0; /*to denote that we are 
				descending along the left*/
    a->result_node = bdd_get_free_node_sequential(bddm_r);
    insert_cache(bddm_r, h, p, q, a->result_node); 
    
    if ((p_i == BDD_LEAF_INDEX) && (q_i == BDD_LEAF_INDEX)){
      /*we are at leaves*/
      node_ptr = &p_table[p];
      LOAD_lr(node_ptr, tmpp, tmp1);
      node_ptr = &q_table[q];
      LOAD_lr(node_ptr, tmpq, tmp1);
      res = a->result_node;
      node_ptr = &bddm_r->node_table[res];
      STR_lri(node_ptr, apply2_leaf_function(tmpp, tmpq), 
	      BDD_USED, BDD_LEAF_INDEX); 
      goto finish;}
    else if (p_i == q_i){
      /*descend in both*/
      /*descend left from p, then right*/ 
      node_ptr = &p_table[p];
      LOAD_lr(node_ptr, p, a->second_p);
      /*descend left from q, then right*/ 
      node_ptr = &q_table[q];
      LOAD_lr(node_ptr, q, a->second_q);
      
      a->index = p_i; /*remember for upcoming store operation*/
      INCREMENT_A; /*prepare recursion*/
      /*now do the apply for the left successor*/
      goto start;}
    else if ((q_i == BDD_LEAF_INDEX) || (p_i < q_i)){
      /*descend in p (which is not terminal), remain in q*/
      /*descend left from p, then right*/ 
      node_ptr = &p_table[p];
      LOAD_lr(node_ptr, p, a->second_p);
      a->second_q = q;
      a->index = p_i;
      INCREMENT_A;
      goto start;}
    else  {
      /*remain in p, descend in q (which is not terminal)*/
      /*descend left from q, then right*/ 
      node_ptr = &q_table[q];
      LOAD_lr(node_ptr, q, a->second_q);
      a->second_p = p;
      a->index = q_i;
      INCREMENT_A;
      goto start;}
    
 second:
    /*we are now done with apply along the left*/
    /*now do the apply along the right*/
    a->inter_m = res;
    p = a->second_p;
    q = a->second_q;
    INCREMENT_A;
    goto start;
    
    /*we are done with both apply's*/
 third:
    node_ptr = &bddm_r->node_table[a->result_node];
    STR_lri(node_ptr, 
	    a->inter_m, res, a->index); /*CACHE MISS STALL*/
    res = a->result_node;
    
 finish:
    if (BOTTOM_A) {
      FREE_A;	
      PUSH_SEQUENTIAL_LIST(bddm_r->roots, unsigned, res);
      return res;
    }
    else {
      DECREMENT_A;
      if (a->inter_m)
	  goto third;
      else
	  goto second;}
  }
}
}

 
/* BDD_PROJECT */

typedef struct {
    int index;
    unsigned p, q, h; /* q == 0 iff we are above variable projected away*/
    unsigned second_p, second_q;
  } activation_record_project;

DECLARE_LOCAL(activation_record_project);

DECLARE_POINTER_TO_LOCAL(activation_record_project, apply_project_ptr);

void update_activation_stack_project(unsigned (*new_place)(unsigned node)) {
  activation_record_project *a_;
  if (apply_project_ptr->bddm_p == apply_project_ptr->bddm_r) {
    for (a_ = apply_project_ptr->act_stack; 
	 a_ <= apply_project_ptr->a ; a_++) {   
      a_->p = new_place(a_->p);
      a_->q = new_place(a_->q);
      a_->h = hash_value_invalid;
      /* a correspond to a leaf, and a_->second_p is only well_defined 
       * for internal nodes */
      if (apply_project_ptr->a != a_) { 
	a_->second_p = new_place(a_->second_p);
	if (a_->second_q != BDD_UNUSED)
	  a_->second_q = new_place(a_->second_q);
      }
    }      
  }
}

 

unsigned bdd_project(bdd_manager *bddm_p, unsigned p, unsigned var_index,
		 bdd_manager *bddm_r,
		 unsigned (*project_leaf_function)(unsigned value1, unsigned value2)) {
  
  unsigned p_i, q_i;
  unsigned res;
  bdd_record *node_ptr;
  unsigned tmp1, tmpp, tmpq;
  unsigned h;
  bdd_record *p_table;
  unsigned q;
  
  res = lookup_cache(bddm_r, &h, p, 0); /*CACHE MISS STALL*/
  if (res) {

#ifdef _BDD_STAT_
    bddm_r->apply1_steps++;
#endif
  
    PUSH_SEQUENTIAL_LIST(bddm_r->roots, unsigned, res);
    return res;
  } 
  else {
    DECLARE_POINTER_TO_LOCAL(activation_record_project, local);
    DECLARE_POINTER_TO_LOCAL(activation_record_project, old_apply_project_ptr);
    DECLARE_SEQUENTIAL_LIST(intermediate, unsigned)
    
    NEW_LOCAL(activation_record_project, local,
	      bddm_p, bddm_p, bddm_r); /* second last argument is dummy */

    MAKE_SEQUENTIAL_LIST(intermediate, unsigned, 1024);

    old_apply_project_ptr = apply_project_ptr;
    apply_project_ptr = local;

  
    q = 0;

  start:
    p_table = bddm_p->node_table;

    node_ptr = &p_table[p];  
    LOAD_index(node_ptr, a->index); /*get index*/ /*CACHE MISS STALL*/
    PUSH_SEQUENTIAL_LIST(intermediate, unsigned, going_left);
  
    if (q != 0) {
      if (p == q) 
	q = 0; /*we are below var_index, but have converged on same node*/
      else
	goto start_p_q_diff;
    } else if (a->index == var_index) { /*split into two calculations*/
      LOAD_lr(node_ptr, p, q);
      node_ptr = &p_table[p];
      goto start_p_q_diff;
    };
  
    a->q = 0; /*to indicate that we are in only one node in bddm_p*/
    
    res = lookup_cache(bddm_r, &h, p, 0); /*CACHE MISS STALL*/
    if (res) 
      goto finish;
    else {
    
#ifdef _BDD_STAT_
      bddm_r->apply1_steps++;
#endif
      a->p = p; /*this for later insert_cache operation*/
      a->h = h; /*this for later insert_cache operation*/
    
      if (a->index == BDD_LEAF_INDEX) {
	/*we are at leaf*/
	LOAD_lr(node_ptr, tmpp, tmp1);
	res = bdd_find_leaf_hashed(bddm_r, project_leaf_function(tmpp, tmpp), 
				   SEQUENTIAL_LIST(intermediate),
				   &update_activation_stack_project); 
	apply_project_ptr = local;
	if (a->h == hash_value_invalid)
	  a->h = HASH2(a->p, a->q, bddm_r->cache_mask); 
	insert_cache(bddm_r, a->h, a->p, 0, res);
	goto finish;}
      else {
	/*descend left from p, then right*/ 
	LOAD_lr(node_ptr, p, a->second_p);
	a->second_q = BDD_UNUSED;
	INCREMENT_LOCAL(local); /*prepare recursion*/
	/*now do the apply for the left successor*/
	goto start;}
    
    second_p:
      /*we are now done with apply along the left*/
      /*now do the apply along the right*/
      p = a->second_p;
      q = 0;
      STORE_TOP_SEQUENTIAL_LIST(intermediate, res);
      INCREMENT_LOCAL(local);
      goto start;
    
      /*we are done with both left and right apply's*/
    third_p:
      if (TOP_SEQUENTIAL_LIST(intermediate) == res) {
	/*then res is the right answer*/
      } else {
	res = bdd_find_node_hashed(bddm_r, TOP_SEQUENTIAL_LIST(intermediate),
				   res, a->index, 
				   SEQUENTIAL_LIST(intermediate),
				   &update_activation_stack_project); /*CACHE MISS STALL*/
      }
      apply_project_ptr = local;
      if (a->h == hash_value_invalid)
	a->h = HASH2(a->p, 0, bddm_r->cache_mask); 
      insert_cache(bddm_r, a->h, a->p, 0, res); /*CACHE MISS STALL*/
      goto finish;
    
    start_p_q_diff:
    
#ifdef _BDD_STAT_
      bddm_r->apply2_steps++;
#endif
    
      /*assertion: node_ptr == &p_table[p] && p != q*/
      a->q = q; 
      res = lookup_cache(bddm_r, &h, p, q); /*CACHE MISS STALL*/
      if (res) 
	goto finish;
      else {
	LOAD_index(node_ptr,p_i); /*get indices*/ /*CACHE MISS STALL*/
	node_ptr = &p_table[q];
	LOAD_index(node_ptr,q_i);                 /*CACHE MISS STALL*/
	a->p = p; /*this for later insert_cache operation*/
	a->h = h;
      
	if ((p_i == BDD_LEAF_INDEX) && (q_i == BDD_LEAF_INDEX)){
	  /*we are at leaves*/
	  node_ptr = &p_table[p];
	  LOAD_lr(node_ptr, tmpp, tmp1);
	  node_ptr = &p_table[q];
	  LOAD_lr(node_ptr, tmpq, tmp1);
	  res = bdd_find_leaf_hashed(bddm_r, project_leaf_function(tmpp, tmpq),
				     SEQUENTIAL_LIST(intermediate),
				     &update_activation_stack_project); 
	  apply_project_ptr = local;
	  if (a->h == hash_value_invalid)
	    a->h = HASH2(a->p, a->q, bddm_r->cache_mask); 
	  insert_cache(bddm_r, a->h, a->p, a->q, res); 
	  goto finish;
	}
	else if (p_i == q_i){
	  /*descend in both*/
	  /*descend left from p, then right*/ 
	  node_ptr = &p_table[p];
	  LOAD_lr(node_ptr, p, a->second_p);
	  /*descend left from q, then right*/ 
	  node_ptr = &p_table[q];
	  LOAD_lr(node_ptr, q, a->second_q);
	
	  a->index = p_i; /*remember for upcoming store operation*/
	  INCREMENT_LOCAL(local); /*prepare recursion*/
	  /*now do the apply for the left successor*/
	  goto start;
	}
	else if ((q_i == BDD_LEAF_INDEX) || (p_i < q_i)){
	  /*descend in p (which is not a leaf), remain in q*/
	  /*descend left from p, then right*/ 
	  node_ptr = &p_table[p];
	  LOAD_lr(node_ptr, p, a->second_p);
	  a->second_q = q;
	  a->index = p_i;
	  INCREMENT_LOCAL(local);
	  goto start;}
	else  {
	  /*remain in p, descend in q (which is not a leaf)*/
	  /*descend left from q, then right*/ 
	  node_ptr = &p_table[q];
	  LOAD_lr(node_ptr, q, a->second_q);
	  a->second_p = p;
	  a->index = q_i;
	  INCREMENT_LOCAL(local);
	  goto start;
	}
      } 
    
    second_p_q_diff:
      /*we are now done with apply along the left*/
      /*now do the apply along the right*/
      p = a->second_p;
      q = a->second_q;
      STORE_TOP_SEQUENTIAL_LIST(intermediate, res);
      INCREMENT_LOCAL(local);
      goto start;
    
      /*we are done with both apply's*/
    third_p_q_diff:
      if (TOP_SEQUENTIAL_LIST(intermediate) == res) {
	/*then res is the right answer*/
      } else    
	res = bdd_find_node_hashed(bddm_r, TOP_SEQUENTIAL_LIST(intermediate), 
				   res, a->index,
				   SEQUENTIAL_LIST(intermediate),
				   &update_activation_stack_project); /*CACHE MISS STALL*/
      apply_project_ptr = local;
      if (a->h == hash_value_invalid)
	a->h = HASH2(a->p, a->q, bddm_r->cache_mask); 
      insert_cache(bddm_r, a->h, a->p, a->q, res); /*CACHE MISS STALL*/
    
    finish:
      if (BOTTOM_LOCAL(local)) {
	FREE_LOCAL(activation_record_project,local);	
	FREE_SEQUENTIAL_LIST(intermediate);
	PUSH_SEQUENTIAL_LIST(bddm_r->roots, unsigned, res);
	apply_project_ptr = old_apply_project_ptr;
	return res;
      }
      else {
	DECREMENT_LOCAL(local);
	PEEL_SEQUENTIAL_LIST(intermediate, unsigned);
	if (a->q) {
	  if (TOP_SEQUENTIAL_LIST(intermediate) == going_left)
	    goto second_p_q_diff;
	  else
	    goto third_p_q_diff;
	}
	else /* a->q == 0 */
	  if (TOP_SEQUENTIAL_LIST(intermediate) == going_left)
	    goto second_p ;
	  else
	    goto third_p;
      
      }
    }
  }
}



/* BDD_OPERATE_ON_NODES */

void bdd_operate_on_nodes(bdd_manager *bddm_p, unsigned p, 
			  void (*operator_function)
			  (bdd_record *node_pointer)) {
  
   bdd_record *node_ptr;
   bdd_record *p_table;
  
  DECLARE_A(activation_record_apply1);

  DECLARE_SEQUENTIAL_LIST(intermediate, unsigned);
  
  
  MAKE_SEQUENTIAL_LIST(intermediate, unsigned, 1024);

 start:
  
#ifdef _BDD_STAT_
  bddm_p->call_steps++;
#endif

  PUSH_SEQUENTIAL_LIST(intermediate, unsigned, going_left);
  
  p_table = bddm_p->node_table;
  node_ptr = &p_table[p];
  
  if (node_ptr->mark) 
      goto finish;
  else {
    LOAD_index(node_ptr,a->index); /*get index*/ /*CACHE MISS STALL*/
    node_ptr->mark = 1; 
    operator_function(node_ptr);
 
    if (a->index == BDD_LEAF_INDEX) {
      /*we are at leaf*/
     goto finish;}
    else {
      a->p = p;
       /*descend left from p, then right*/ 
      LOAD_lr(node_ptr, p, a->second);
      INCREMENT_A; /*prepare recursion*/
      /*now do the calls for the left successor*/
      goto start;}
    
  second:
    /*we are now done with calls along the left*/
    /*now do the calls along the right*/
    STORE_TOP_SEQUENTIAL_LIST(intermediate, 1);
    p = a->second;
    INCREMENT_A;
    goto start;
    
    /*we are done with both left and right calls*/
  third: {}
   
  finish:
    if (BOTTOM_A) {
      FREE_A;
      FREE_SEQUENTIAL_LIST(intermediate);
      return;
    }
    else {
      DECREMENT_A;
      PEEL_SEQUENTIAL_LIST(intermediate, unsigned);
      if (TOP_SEQUENTIAL_LIST(intermediate) == going_left)
	  goto second; 
      else
	  goto third;
    }
  }
}

/* BDD_CALL_LEAFS */

void (*leaf_function_global)(unsigned value);

void bbd_operate_on_leaf (bdd_record *node_pointer) {
  unsigned indx, tmpp, tmp1;
  LOAD_index(node_pointer, indx); 
  if (indx == BDD_LEAF_INDEX) {
    LOAD_lr(node_pointer, tmpp, tmp1);
    (*leaf_function_global) (tmpp);
  }
}

void bdd_call_leafs(bdd_manager *bddm_p, unsigned p, 
		    void (*leaf_function)(unsigned value)) {
  leaf_function_global = leaf_function;
  bdd_operate_on_nodes (bddm_p, p, bbd_operate_on_leaf);
}

/* BDD_REPLACE_INDICES */

unsigned *indices_map_global;

void bbd_replace_index (bdd_record *node_pointer) {
  unsigned indx, l, r;
  LOAD_lri(node_pointer, l, r, indx); 
  if (indx != BDD_LEAF_INDEX) {
    invariant(indices_map_global[indx] <= BDD_MAX_INDEX);
    STR_lri(node_pointer, l, r, indices_map_global[indx]);
  }
}

void bdd_replace_indices (bdd_manager *bddm_p, unsigned p, 
			  unsigned indices_map []) {
  indices_map_global = indices_map;
  bdd_operate_on_nodes (bddm_p, p, bbd_replace_index);
}

extern unsigned fn_identity(unsigned p)
{
  return p;
}
