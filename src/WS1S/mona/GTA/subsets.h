/* subsets.h */

/*
 * MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
 *
 * Reproduction of all or part of this software is permitted for
 * educational or research use on condition that this copyright notice is
 * included in any copy. This software comes with no warranty of any
 * kind. In no event will BRICS be liable for any damages resulting from
 * use of this software.
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
