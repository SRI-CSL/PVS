/* bdd_example.c - BDD package example application */

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
#include <assert.h>
#include "bdd.h"

unsigned and(unsigned a, unsigned b) { 
  if (a && b) 
    return 1;
  else 
    return 0; 
};

unsigned not(unsigned a) { 
  if (a)
    return 0;
  else 
    return 1; 
};

void print_bdd(bdd_manager *bddm, bdd_ptr b) {
  unsigned index;
  
  if (bdd_is_leaf(bddm, b)) {
    printf("(leafvalue: %d)", bdd_leaf_value(bddm, b));
  }
  else {
    index=bdd_ifindex(bddm,b);
    printf("(node %d, indx %d, high:", b, index);
    print_bdd(bddm, bdd_then(bddm,b));
    printf(")");
    printf("(node %d, indx %d, low:", b, index);
    print_bdd(bddm, bdd_else(bddm,b));
    printf(")");
  };
}

int main() {
  bdd_manager *bddm, *bddm1;
  bdd_ptr zero, one;
  bdd_handle var2, var7;
  bdd_ptr and_2_7, nand_2_7;
  bdd_handle handle;

  bdd_init(); /* needed since we're using statistics */

  bddm = bdd_new_manager(100,50);
  /* get a BDD pointer to a node that is the leaf with value 0 */
  zero = bdd_find_leaf_hashed_add_root(bddm, 0);
  /* and a leaf with value 1 */
  one =  bdd_find_leaf_hashed_add_root(bddm, 1);
  /* note already at this point "zero" could have been invalidated if
     the table doubled, but we know that there is room for a 100
     nodes---anyway, this is really very bad style, so we go on in
     a more appropriate manner */

  /* "then" part is one, "else" part is zero */
  var2 = bdd_handle_find_node_hashed_add_root(bddm, zero, one, 2);
  var7 = bdd_handle_find_node_hashed_add_root(bddm, zero, one, 7);
  
  /* check node pointers and handles */
  assert(zero == BDD_ROOT(bddm, 0)); /* since table was not doubled */
  assert(one  == BDD_ROOT(bddm, 1)); 
  assert(var2 == 2);
  assert(var7 == 3);

  bddm1 = bdd_new_manager(100,50); /* make room for at least 100 nodes,
				      overflow increment is 50 */
  
  bdd_make_cache(bddm1, 100, 50); /* apply2 needs a result cache, here the size
				     is a hundred with increment 50 */

  /* apply operation on var2 and var7 in bddm; the result is 
     a completely fresh bdd in bddm1 and a BDD pointer, named "and_2_7" */
  and_2_7 = bdd_apply2_hashed(bddm, BDD_ROOT(bddm, var2), /* BDD #1 */
      			      bddm, BDD_ROOT(bddm, var7), /* BDD #2 */
			      bddm1, /* result BDD */
			      &and); /* leaf operation */
  
  bdd_update_statistics(bddm, 0); /* update statics group "0" with data from bddm 
				     before killing the manager */
 
  printf("Size of bddm: %d\n\n", bdd_size(bddm)); /* let's see the number of nodes created */
 
  bdd_kill_manager(bddm);
 
  printf("Size of bddm1: %d\n\n", bdd_size(bddm1));
 
  handle = BDD_LAST_HANDLE(bddm1); 

  assert(handle == 0);
  assert(BDD_ROOT(bddm1, handle) == and_2_7);

  /* reset all mark fields in bddm1 before an apply1 operation */
  bdd_prepare_apply1(bddm1); 
  
  /* a new bdd (which as an unlabelled graph is isomorphic to old one)
     in bddm1 is the result of the following apply operation */

  /* it's safe here to use and_2_7 since no operations were performed
     after it was calculated that could have entailed doubling of table */
  nand_2_7 = bdd_apply1(bddm1, and_2_7, bddm1, &not);
 
  bdd_update_statistics(bddm1, 1);
  printf("Size of bddm1: %d\n\n", bdd_size(bddm1));
  bdd_kill_manager(bddm);

  print_bdd(bddm1, and_2_7);
  printf("\n\n");
  print_bdd(bddm1, nand_2_7);
  printf("\n\n");

  bdd_print_statistics(0, "bddm"); /* print group 0 statistics with heading "bddm" */
  bdd_print_statistics(1, "bddm1"); /* print group 1 statistics with heading "bddm1" */
  return 0;
 };
