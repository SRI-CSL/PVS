/* bdd_dump.c */

/*
 * MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
 *
 * Reproduction of all or part of this software is permitted for
 * educational or research use on condition that this copyright notice is
 * included in any copy. This software comes with no warranty of any
 * kind. In no event will BRICS be liable for any damages resulting from
 * use of this software.
 */

#include <stdio.h>
#include "bdd_dump.h"

void bddReverseMarks(bdd_manager *bddm, bdd_ptr p)
{
  if ((signed) bdd_mark(bddm, p) < 0) {
    bdd_set_mark(bddm, p, ~bdd_mark(bddm, p));
    if (!bdd_is_leaf(bddm, p)) {
      bddReverseMarks(bddm, ws1s___bdd_else(bddm, p)); 
      bddReverseMarks(bddm, ws1s___bdd_then(bddm, p)); 
    }
  }
}

void bddDumpNode(bdd_manager *bddm, bdd_ptr p)
{
  if ((signed) bdd_mark(bddm, p) >= 0) {
    bdd_set_mark(bddm, p, ~bdd_mark(bddm, p));
    if (!bdd_is_leaf(bddm, p)) {
      printf("%-3u: idx=%-3u lo=%-3u hi=%-3u\n", 
	     p,
	     bdd_ifindex(bddm, p),
	     ws1s___bdd_else(bddm, p),
	     ws1s___bdd_then(bddm, p));
      bddDumpNode(bddm, ws1s___bdd_else(bddm, p)); 
      bddDumpNode(bddm, ws1s___bdd_then(bddm, p)); 
    }
    else
      printf("%-3u: state=%-3u\n", p, bdd_leaf_value(bddm, p));
  }
}

void bddDump(bdd_manager *bddm)
{
  int i;
  printf("\nBDD DUMP:\n");
  for (i = 0; i < bdd_roots_length(bddm); i++)
    bddDumpNode(bddm, BDD_ROOT(bddm, i));
  printf("END\n\n");
  for (i = 0; i < bdd_roots_length(bddm); i++)
    bddReverseMarks(bddm, BDD_ROOT(bddm, i));
}
