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

#ifndef __SUBSETS_H
#define __SUBSETS_H

#include "gta.h"

typedef struct SubsetsEntry { 
  unsigned n; /* invariant: n > elements, c1 and c2 */
  unsigned c1, c2; /* immediate constituents */
  unsigned *elements; /* sorted array */
  unsigned length; /* number of elements */
  struct SubsetsEntry *overflow;
} SubsetsEntry;

typedef struct {
  SubsetsEntry *t; /* map from elements to n */
  SubsetsEntry **inverse; /* map from n to elements */
  unsigned size, overflows, prime;
  unsigned inverseAllocated;
  unsigned num; /* number of subsets excluding singletons */
  unsigned singletons; /* number of singleton sets */
} Subsets;

void ssInit(Subsets *s, unsigned singletons, unsigned initialCapacity);
void ssFree(Subsets *s);
int ssLookupAndInsert(Subsets *s, unsigned c1, unsigned c2, unsigned *n);
unsigned ssSize(Subsets *s);
void ssGetComponents(Subsets *s, unsigned n, unsigned *c1, unsigned *c2);

#ifndef NDEBUG
void ssDump(Subsets *s);
#endif

#endif
