/* makebasic.c */

/*
 * MONA is Copyright (C) 1997 BRICS. All rights reserved. 
 *
 * Reproduction of all or part of this software is permitted for
 * educational or research use on condition that this copyright notice is
 * included in any copy. This software comes with no warranty of any
 * kind. In no event will BRICS be liable for any damages resulting from
 * use of this software.
 */

#include <stdio.h>
#include <stdlib.h>
#include "dfa.h"
#include "bdd_internal.h"

struct path_descr {
  int value;
  char path[10];
};

int exception_index, no_exceptions;
struct path_descr exceptions[10];

void dfaAllocExceptions(int n)
{
  no_exceptions = n;
  exception_index = 0;
}

void dfaStoreException(int value, char *path)
{
  exceptions[exception_index].value = value;
  strcpy(exceptions[exception_index].path, path);
  
  exception_index++;
}

int no_states;
unsigned default_state;

unsigned unite_leaf_fn(unsigned p_value, unsigned q_value)
{
/*
  printf("unite_leaf_fn with p_value = %d and q_value = %d\n", p_value, q_value);
*/
  if ((p_value == q_value) || (q_value == default_state))
    return p_value;
  else if (p_value == default_state)
    return q_value;
  else {
    printf("Error in unite\n");
    exit (-1);
  }
}


/* Prerequisite: bddm->roots_array is non empty */
bdd_ptr unite_roots(bdd_manager *bddm)
{
  int no_roots = bddm->roots_index;
  int i;
  bdd_ptr result;

  if ( !(result = bddm->roots_array[0]) ) {
    printf("Error in unite: no roots to unite.\n");
    exit (-1);
  }

  /* 
   * Number of iterations must be fixed/controlled, since
   * bdd_apply2_hashed add roots
   */
  for (i = 1; i < no_roots; i++)
    result = bdd_apply2_hashed(bddm, result, 
			       bddm, bddm->roots_array[i],
			       bddm, &unite_leaf_fn);
  
  return result;
}

int sorted_indices[10];    /* holds indices, which order the offsets argument to dfaBuild */
int global_offsets[10];       /* holds the offsets argument to dfaBuild */
int offsets_size;          /* holds the offsets_size argument to dfaBuild */
char sorted_path[10];         /* holds the current exception path, sorted according to the offsets */

DECLARE_SEQUENTIAL_LIST(sub_results, unsigned)

bdd_ptr makepath(bdd_manager *bddm, int n, unsigned leaf_value, 
		  void (*update_bddpaths) (unsigned (*new_place) (unsigned node)))
{
  bdd_ptr res, sub_res, default_state_ptr;
  unsigned index;

  while ((n < offsets_size) && (sorted_path[n] == 'X'))
    n++;

  if (n >= offsets_size)
    return (bdd_find_leaf_hashed(bddm, leaf_value, SEQUENTIAL_LIST(sub_results), update_bddpaths));

  sub_res = makepath(bddm, n+1, leaf_value, update_bddpaths);
  PUSH_SEQUENTIAL_LIST(sub_results, unsigned, sub_res);
  default_state_ptr = bdd_find_leaf_hashed(bddm, default_state, SEQUENTIAL_LIST(sub_results), update_bddpaths);
  POP_SEQUENTIAL_LIST(sub_results, unsigned, sub_res);

  index = global_offsets[sorted_indices[n]];
  
  if (sorted_path[n] == '0')
    res = bdd_find_node_hashed(bddm, sub_res, default_state_ptr, index, SEQUENTIAL_LIST(sub_results), update_bddpaths);
  else
    res = bdd_find_node_hashed(bddm, default_state_ptr, sub_res, index, SEQUENTIAL_LIST(sub_results), update_bddpaths);

  return res;
}

int exp_count;
bdd_ptr bddpaths[10];

void update_bddpaths(unsigned (*new_place) (unsigned node)) 
{
  int j;
  
  for (j = 0; j < exp_count; j++) 
    bddpaths[j] = new_place(bddpaths[j]);
}

void makebdd (bdd_manager *bddm)
{
  bdd_manager *tmp_bddm;
  bdd_ptr united_bdds, default_ptr;
  int i;

  tmp_bddm = bdd_new_manager(8, 4);

  /*
  ** insert a leaf with value 'default_state' in tmp_bddm,
  ** if not already present
  */
  default_ptr = bdd_find_leaf_hashed(tmp_bddm, default_state, SEQUENTIAL_LIST(sub_results), &update_bddpaths); 

  for (exp_count = 0; exp_count < no_exceptions; exp_count++) {
    for (i = 0; i < offsets_size; i++)
      sorted_path[i] = exceptions[exp_count].path[sorted_indices[i]];

    /* clear the cache */
    bdd_kill_cache(tmp_bddm);
    bdd_make_cache(tmp_bddm, 8, 4);
    tmp_bddm->cache_erase_on_doubling = TRUE;

    bddpaths[exp_count] = makepath(tmp_bddm, 0, exceptions[exp_count].value, &update_bddpaths);
    PUSH_SEQUENTIAL_LIST(tmp_bddm->roots, unsigned, bddpaths[exp_count]);
  }    

  if (no_exceptions == 0)
    united_bdds = default_ptr;
  else if (no_exceptions == 1) 
    united_bdds = TOP_SEQUENTIAL_LIST(tmp_bddm->roots);
  else
    united_bdds = unite_roots(tmp_bddm);

  bdd_prepare_apply1(tmp_bddm);
  bdd_apply1(tmp_bddm, united_bdds, bddm, &fn_identity);       /* store the result in bddm->roots */

  bdd_kill_manager(tmp_bddm);
}

int offsets_cmp(const void *index1, const void *index2) 
{
  int o1, o2;
  
  o1 = global_offsets[*((int *)index1)];
  o2 = global_offsets[*((int *)index2)];
  
  if (o1 < o2) return -1;
  else if (o1 == o2) return 0;
  else return 1;
}

DFA *aut;

void dfaSetup(int ns, int os, int *offsets)
{
  int i;

  MAKE_SEQUENTIAL_LIST(sub_results, unsigned, 64);
  
  no_states = ns;

  offsets_size = os;
  for (i = 0; i < offsets_size; i++) {
    sorted_indices[i] = i;
    global_offsets[i] = offsets[i];
  }

  qsort(sorted_indices, offsets_size, sizeof(int), &offsets_cmp);

  aut = dfaMake(no_states);

  aut->ns = no_states;
  aut->s = 0;
}

void dfaStoreState(int ds)
{
  default_state = ds;

  bdd_kill_cache(aut->bddm);
  bdd_make_cache(aut->bddm, 8, 4);

  makebdd(aut->bddm);
}

DFA *dfaBuild(char *finals)
{
  int        i;
  unsigned  *root_ptr;

  for (i=0, root_ptr = bdd_roots(aut->bddm); i < no_states; root_ptr++, i++) {
    aut->q[i] = *root_ptr;
    aut->f[i] = (finals[i] == '-') ? -1 : (finals[i] == '+' ? 1 : 0);
  }

  FREE_SEQUENTIAL_LIST(sub_results);

  return aut;
}
