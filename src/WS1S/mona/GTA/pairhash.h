/* pairhash.h */

/*
 * MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
 *
 * Reproduction of all or part of this software is permitted for
 * educational or research use on condition that this copyright notice is
 * included in any copy. This software comes with no warranty of any
 * kind. In no event will BRICS be liable for any damages resulting from
 * use of this software.
 */

/* hash tables for mapping from state pairs to new states */

typedef struct PairHashTableEntry {
  unsigned p, q, n;
  struct PairHashTableEntry *overflow;
} PairHashTableEntry;

typedef struct {
  PairHashTableEntry *t;
  unsigned size, overflows, prime;
} PairHashTable;

void initPHT(PairHashTable *t, unsigned prime);
void freePHT(PairHashTable *t);
int lookupPHT(PairHashTable *t, unsigned p, unsigned q, unsigned *n);
void insertPHT(PairHashTable *t, unsigned p, unsigned q, unsigned n);

#ifndef NDEBUG
void dumpPHT(PairHashTable *t);
#endif
