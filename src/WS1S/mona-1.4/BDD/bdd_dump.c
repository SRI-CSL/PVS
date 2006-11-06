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
#include "bdd_dump.h"

void bddReverseMarks(bdd_manager *bddm, bdd_ptr p)
{
  if ((signed) bdd_mark(bddm, p) < 0) {
    bdd_set_mark(bddm, p, ~bdd_mark(bddm, p));
    if (!bdd_is_leaf(bddm, p)) {
      bddReverseMarks(bddm, mona_bdd_else(bddm, p)); 
      bddReverseMarks(bddm, mona_bdd_then(bddm, p)); 
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
	     mona_bdd_else(bddm, p),
	     mona_bdd_then(bddm, p));
      bddDumpNode(bddm, mona_bdd_else(bddm, p)); 
      bddDumpNode(bddm, mona_bdd_then(bddm, p)); 
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
