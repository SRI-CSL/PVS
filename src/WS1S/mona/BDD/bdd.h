/*
 * MONA is Copyright (C) 1997 BRICS. All rights reserved. 
 *
 * Reproduction of all or part of this software is permitted for
 * educational or research use on condition that this copyright notice is
 * included in any copy. This software comes with no warranty of any
 * kind. In no event will BRICS be liable for any damages resulting from
 * use of this software.
 */

#ifndef _BDD_PACKAGE
#define _BDD_PACKAGE

#include <stdio.h>

/*CUSTOMIZED CONSTANTS*/


/* before a bdd manager is killed, its statistics can be added to the
   i'th statistics group, where i is less than BDD_STAT_INDEX_SIZE */

#define BDD_STAT_INDEX_SIZE 3

#define _BDD_STAT_

/*MISCELLANEOUS DEFINITIONS*/

extern int memlimit;

typedef unsigned boolean;
#define TRUE 1
#define FALSE 0

#define BDD_MAX_TOTAL_TABLE_SIZE 0x1000000 
/* = 2^24 corresponding to three bytes */

#define BDD_MAX_INDEX 0xfffe
/* = 0xfffe (since 0xffff = BDD_LEAF_INDEX denotes a leaf) */

/*BDD DATA STRUCTURES AND ELEMENTARY OPERATIONS*/

/* convention: "left" means "low" successor (corresponding to false)
   "right" means "high" successor (corresponding to true) */

typedef unsigned bdd_ptr; /* a BDD node is identified by an unsigned
			     offset into the node table, so this is
			     not really a pointer */
 
/* each BDD node sits in the hashed node table and consists of four
   32-bit words */
/* macros for getting at the packed fields are given at the end of this
   file */
struct bdd_record_ {
  unsigned lri[2]; /*left and right, of type bdd_ptr, each consists of
 		    three bytes, and index (name of variable) is
		    a two byte an integer */
  bdd_ptr next;   /*only the next field of 0th record in a bucket is used*/
  unsigned mark;   /*this field is used in apply1 as a bdd_ptr;it is
		     also used in other routines such as
		     bdd_operate_on_nodes */
};

typedef struct bdd_record_ bdd_record;

typedef unsigned bdd_handle; /* an offset into the bdd_roots array */

/* BDD MANAGER */

typedef struct bdd_manager_ bdd_manager;
 
/* MANAGER AND CACHE */

extern bdd_manager *bdd_new_manager(unsigned table_size, 
				    unsigned table_overflow_increment);

extern void bdd_kill_manager(bdd_manager *bddm); 

extern void bdd_make_cache(bdd_manager *bddm, unsigned size, 
			    unsigned overflow_increment);

extern void bdd_kill_cache(bdd_manager *bddm);


extern unsigned bdd_size(bdd_manager *bbdm);

/* STATISTICS */
 
extern void bdd_init(void);

extern void bdd_update_statistics(bdd_manager *bddm, unsigned stat_index); 

extern void bdd_print_statistics(unsigned stat_index, char info[]);

/* SIMPLE BDD OPERATIONS */

extern unsigned bdd_ifindex(bdd_manager *bbdm, bdd_ptr p);

extern bdd_ptr bdd_then (bdd_manager *bbdm, bdd_ptr p);

extern bdd_ptr bdd_else(bdd_manager *bbdm, bdd_ptr p) ;

extern unsigned bdd_is_leaf(bdd_manager *bbdm, bdd_ptr p);

extern unsigned bdd_leaf_value(bdd_manager *bbdm, bdd_ptr p);

/* sequential access */

extern bdd_ptr bdd_get_free_node_sequential(bdd_manager *bbdm);

extern bdd_ptr bdd_find_node_sequential(bdd_manager *bbdm,
				  bdd_ptr l, bdd_ptr r, unsigned i);

extern bdd_ptr bdd_find_leaf_sequential(bdd_manager *bbdm, unsigned val);

/* hashed access */

extern bdd_ptr bdd_find_node_hashed(bdd_manager *bddm,
				     bdd_ptr l, bdd_ptr r, unsigned indx,
				     bdd_ptr *some_roots,
				     void (*update_fn)
				     (bdd_ptr (*new_place)(bdd_ptr node)));

extern bdd_ptr bdd_find_node_hashed_add_root(bdd_manager *bddm,
				     bdd_ptr l, bdd_ptr r, unsigned indx);

extern bdd_handle bdd_handle_find_node_hashed_add_root(bdd_manager *bddm,
				     bdd_ptr l, bdd_ptr r, unsigned indx);

extern bdd_ptr bdd_find_leaf_hashed(bdd_manager *bddm, unsigned val,
				     void *some_roots,
				     void (*update_fn)
				     (bdd_ptr (*new_place)(bdd_ptr node)));

extern bdd_ptr bdd_find_leaf_hashed_add_root(bdd_manager *bddm, unsigned val);

extern bdd_handle bdd_handle_find_leaf_hashed_add_root(bdd_manager *bddm, unsigned val);


/* COMPLEX OPERATIONS (UNARY) */
 
extern void bdd_prepare_apply1(bdd_manager *bddm);

extern bdd_ptr bdd_apply1(bdd_manager *bddm, bdd_ptr p, 
			   bdd_manager *bbdm_r, 
			   unsigned (*apply1_leaf_function)(unsigned value));

/* call *operator_function for each node accessible from p */
extern void bdd_operate_on_nodes(bdd_manager *bddm_p, bdd_ptr p, 
			  void (*operator_function)
			  (bdd_record *node_pointer));
  
/* call *leaf_function for each leaf accessible from p */
extern void bdd_call_leafs(bdd_manager *bddm, bdd_ptr p, 
			   void (*leaf_function)(unsigned value));

/* replace the index i of any internal node accessible from p by
   indices_map[i] */
extern void bdd_replace_indices (bdd_manager *bddm_p,
				 bdd_ptr p, unsigned indices_map []);

/* COMPLEX OPERATIONS (BINARY) */

extern bdd_ptr bdd_apply2_sequential(bdd_manager *bddm_p, bdd_ptr p, 
				     bdd_manager *bddm_q, bdd_ptr q,
				     bdd_manager *bbdm_r,
				     unsigned (*apply2_leaf_function)
				     (unsigned p_value, unsigned q_value));

extern bdd_ptr bdd_apply2_hashed(bdd_manager *bddm_p, bdd_ptr p, 
				  bdd_manager *bddm_q, bdd_ptr q,
				  bdd_manager *bbdm_r,
				  unsigned (*apply2_leaf_function)
				  (bdd_ptr p_value, bdd_ptr q_value));

extern bdd_ptr bdd_project(bdd_manager *bddm_p, bdd_ptr p, unsigned var_index,
			    bdd_manager *bddm_r,
			    unsigned (*project_leaf_function)(unsigned value1, unsigned value2));


/* BDD_ROOTS */ 

extern bdd_ptr *bdd_roots(bdd_manager *bddm); /* SHOULD NOT BE USED */

#define BDD_ROOTS(bddm) \
  SEQUENTIAL_LIST(bddm->roots)

extern unsigned bdd_roots_length(bdd_manager *bddm); /* SHOULD NOT BE USED */

#define BDD_ROOT(bddm, handle) \
  SEQUENTIAL_LIST(bddm->roots)[handle]

#define BDD_ADD_ROOT(bddm, p) \
 PUSH_SEQUENTIAL_LIST(bddm->roots, unsigned, p)

#define BDD_LAST_HANDLE(bddm) \
 (LENGTH_SEQUENTIAL_LIST(bddm->roots) - 1)

#define BDD_ADD_ROOT_SET_HANDLE(bddm, p, h) \
 BDD_ADD_ROOT(bddm, p) \
 h = BDD_LAST_HANDLE(bddm) 

/* MARK STUFF */

extern unsigned bdd_mark(bdd_manager *bddm, bdd_ptr p);

extern void bdd_set_mark(bdd_manager *bddm, bdd_ptr p, unsigned mark);


/* PRINT */

extern void print_bddpaths(bdd_ptr p, bdd_ptr q, bdd_manager *bddm,
			   unsigned b, unsigned no_free_vars, unsigned *offsets);

extern void print_bddpaths_verbose(bdd_ptr p, bdd_ptr q, 
				   bdd_manager *bddm, unsigned b);


/* BDD TRACE */

struct trace_descr_ {
  unsigned index;
  boolean value;
  struct trace_descr_ *next;
};

typedef struct trace_descr_ *trace_descr;

struct paths_ {
  unsigned to;
  trace_descr trace;
  struct paths_ *next;
};
 
typedef struct paths_ *paths;

paths make_paths(bdd_manager *bddm, bdd_ptr p);
void kill_trace(trace_descr t);
void kill_paths(paths p);
trace_descr find_one_path(bdd_manager *bddm, bdd_ptr p, bdd_ptr q);

void print_bddpaths(bdd_ptr p, bdd_ptr q, bdd_manager *bddm, unsigned b, unsigned no_free_vars, unsigned *offsets);

void print_bddpaths_verbose(bdd_ptr p, bdd_ptr q, bdd_manager *bddm, unsigned b);

void print_one_path(bdd_ptr p, bdd_ptr q, bdd_manager *bddm, unsigned no_free_vars, unsigned *offsets);

/* BDD NODES INTERNALS */

/* invariant throughout hashed use: the r-field is different from
   BDD_UNUSED (0) if and only if the record is in use*/

#define BDD_UNUSED 0
#define BDD_USED 1

/*the index of an "undefined" BDD node*/
#define BDD_UNDEF (unsigned) -1

/*value of variable index in BDD node used to indicate that node is a leaf*/
#define BDD_LEAF_INDEX  ((unsigned) 0xffff)

/* look up the two lri fields in a bdd_record_ and extract the value of
   the left child, the right child, and the node index */
#define LOAD_lri(node_ptr, l, r, i)\
i = (node_ptr)->lri[1];\
r = i>>16;\
i &= 0xffff;\
l = (node_ptr)->lri[0];\
r |= ((l & 0xff) <<  16);\
l = l >> 8;\

#define LOAD_lr(node_ptr, l, r)\
l = (node_ptr)->lri[0];\
r = (l << 16) & 0x00ff0000;\
l = l >> 8;\
r |= ((node_ptr)->lri[1] >> 16);\

#define LOAD_index(node_ptr, i)\
i = ((node_ptr)->lri[1] & 0xffff);\

#define LOAD_r(node_ptr)\
((((node_ptr)->lri[0] << 16) & 0x00ff0000) | ((node_ptr)->lri[1] >> 16))\

#define STR_lri(node_ptr, l, r, i)\
(node_ptr)->lri[0] = (l << 8) | (r >> 16);\
(node_ptr)->lri[1] = ((r & 0x0000ffff) << 16) | i;\

/* pack l, r, and i in two unsigned words (corresponding to the lri[0]
   and lri[1] fields */
#define TWO_UNS_STR_lri(i0, i1, l, r, i)\
i0 = (l << 8) | (r >> 16);\
i1 = ((r & 0x0000ffff) << 16) | i;\

/* SEQUENTIAL LISTS */

/* results of BDD operations are stored in lists implemented as
   sequential arrays */

#define DECLARE_SEQUENTIAL_LIST(name, element_type) \
element_type *name##_array;  \
unsigned name##_length; \
unsigned name##_index; \

#define MAKE_SEQUENTIAL_LIST(name, element_type, size) \
name##_array  = (element_type *) mem_get_block((SIZE_T) size * (sizeof (element_type))); \
name##_length = size; \
name##_index = 0; \
name##_array[0] = (element_type) 0; \

#define FREE_SEQUENTIAL_LIST(name) \
mem_free_block(name##_array) \

#define PUSH_SEQUENTIAL_LIST(name, element_type, element) \
if (!(name##_index < name##_length - 1)) { \
  name##_array = (element_type *) mem_resize_block(name##_array, \
           (SIZE_T)((name##_length *= 2) * (sizeof (element_type)))); \
} \
name##_array[name##_index] =  element; \
name##_array[++name##_index] = (element_type) 0; \
	 
#define POP_SEQUENTIAL_LIST(name, element_type, res) \
{res = name##_array[--name##_index]; \
name##_array[name##_index] = (element_type) 0;} \

#define PEEL_SEQUENTIAL_LIST(name, element_type) \
name##_array[--name##_index] = (element_type) 0; \


#define STORE_TOP_SEQUENTIAL_LIST(name, element) \
name##_array[name##_index - 1] = element; \

#define TOP_SEQUENTIAL_LIST(name) \
name##_array[name##_index - 1]  \
	 
#define SEQUENTIAL_LIST(name) \
name##_array	
      
#define  LENGTH_SEQUENTIAL_LIST(name) \
(name##_index)

/* complete definition of bdd_manager */

typedef struct cache_record_  cache_record; /*defined in bdd_internal.h*/

struct bdd_manager_ {
  /* table */

  unsigned table_log_size; 
  unsigned table_size; /* = 2^table_log_size*/
  unsigned table_total_size; /* including overflow area */
  unsigned table_mask; /*table_log_size  - the number of bits in table_mask
			 is the logarithm of the number of bins per 
			 hash address*/
  unsigned table_overflow_increment; /*the number of new nodes added when
				       overflow area is full; also  initial
				       size of overflow area*/
  unsigned table_elements; /*number of elements inserted in hashed mode*/

  bdd_ptr  table_next; /*next available position when nodes are inserted
			 sequentially*/
  bdd_ptr  table_overflow; /*next free node in overflow area
			     when node_table used in hashed mode*/
  unsigned table_double_trigger; /* when to trigger doubting of the table */
  bdd_record *node_table; /*node_table is the beginning of array of BDD nodes*/
  DECLARE_SEQUENTIAL_LIST(roots, unsigned) /*results of applys and projects*/
  /* cache */

  cache_record *cache; /*cache is the beginning of cache table*/
  unsigned cache_total_size; /*size of hashed area + overflow area*/
  unsigned cache_size; /*size of hashed area (which is initialized to
			 unused)*/
  unsigned cache_mask; /*table_log_size  - the number of bits in cache_mask
			 is the logarithm of the number of bins per 
			 hash address*/
  unsigned cache_overflow_increment; /*initial size of overflow area and
				       increment when full*/
  unsigned cache_overflow; /*points to next available
			     position in overflow area*/
  boolean cache_erase_on_doubling; /*if not set, cache is rehashed when table
				     is doubled in hashed access mode; 
				     default is true*/

  /* statistics */

  unsigned number_double;
  unsigned number_cache_collissions;
  unsigned number_cache_link_followed;
  unsigned number_node_collissions;
  unsigned number_node_link_followed;
  unsigned number_lookup_cache;
  unsigned number_insert_cache;
  unsigned apply1_steps;
  unsigned call_steps;
  unsigned apply2_steps;
}; 

extern unsigned fn_identity(unsigned p);

void *mem_alloc(unsigned n);
void *mem_realloc(void *p, unsigned n);
void mem_error();

/* invariant check - can't be disabled */

#define invariant(exp) \
  ((void) ((exp) ? 0 : \
  (printf("%s:%u: failed invariant\n", __FILE__, __LINE__), \
  abort(), 0)))

#endif
