/* pairhash.c */

/*
 * MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
 *
 * Reproduction of all or part of this software is permitted for
 * educational or research use on condition that this copyright notice is
 * included in any copy. This software comes with no warranty of any
 * kind. In no event will BRICS be liable for any damages resulting from
 * use of this software.
 */

#include <stdlib.h>
#include <stdio.h>
#include "pairhash.h"
#include "gta.h"

extern long primes[]; /* defined in ../BDD/hash.c */

#define hashPHT(size, p, q) ((((p)*46349 + (q)) *  67108859) % size)

void initPHT(PairHashTable *t, unsigned prime)
{
  int i;
  t->prime = prime;
  t->size = primes[prime];
  t->overflows = 0;
  t->t = (PairHashTableEntry *) mem_alloc(sizeof(PairHashTableEntry)*t->size);
  for (i = 0; i < t->size; i++) {
    t->t[i].p = -1; /* free mark */
    t->t[i].overflow = 0;
  }
}

void freePHT(PairHashTable *t)
{
  int i;
  for (i = 0; i < t->size; i++) {
    PairHashTableEntry *e = t->t[i].overflow, *w;
    while (e) {
      w = e->overflow;
      free(e);
      e = w;
    }
  }
  free(t->t);
}

int lookupPHT(PairHashTable *t, unsigned p, unsigned q, unsigned *n)
{
  PairHashTableEntry *e = &t->t[hashPHT(t->size, p, q)];
  do {
    if (e->p == p && e->q == q) {
      *n = e->n;
      return 1;
    }
    e = e->overflow;
  } while (e);
  return 0;
}

void insertPHT(PairHashTable *t, unsigned p, unsigned q, unsigned n)
{
  PairHashTableEntry *e = &t->t[hashPHT(t->size, p, q)];

  if (e->p != -1) { /* no room at front? */
    if (t->overflows > t->size*2) { /* too many overflows, rehash */
      unsigned newsize = primes[++(t->prime)];
      PairHashTableEntry *r = 
	(PairHashTableEntry *) mem_alloc(sizeof(PairHashTableEntry)*newsize);
      int i;
      t->overflows = 0;
      for (i = 0; i < newsize; i++) { /* clear all */
	r[i].p = -1;
	r[i].overflow = 0;
      }
      for (i = 0; i < t->size; i++) { /* rehash */
	PairHashTableEntry *w = &t->t[i];
	if (w->p != -1)
	  while (w) {
	    PairHashTableEntry *s = &r[hashPHT(newsize, w->p, w->q)];
	    if (s->p != -1) { /* find back of list */
	      while (s->overflow)
		s = s->overflow;
	      s->overflow = 
		(PairHashTableEntry *) mem_alloc(sizeof(PairHashTableEntry));
	      s = s->overflow;
	      t->overflows++;
	    }
	    s->p = w->p;
	    s->q = w->q;
	    s->n = w->n;
	    s->overflow = 0;
	    w = w->overflow;
	  }
      }
      for (i = 0; i < t->size; i++) { /* free */
	PairHashTableEntry *e = t->t[i].overflow, *w;
	while (e) {
	  w = e->overflow;
	  free(e);
	  e = w;
	}
      }
      free(t->t);
      t->t = r;
      t->size = newsize;
      e = &t->t[hashPHT(t->size, p, q)];
    }
    if (e->p != -1) { /* still no room at front? */
      while (e->overflow) /* place at back of list */
	e = e->overflow;
      e->overflow = 
	(PairHashTableEntry *) mem_alloc(sizeof(PairHashTableEntry));
      e = e->overflow;
      t->overflows++;
    }
  }
  e->p = p;
  e->q = q;
  e->n = n;
  e->overflow = 0;
}

#ifndef NDEBUG
void dumpPHT(PairHashTable *t)
{
  int i;
  printf("\n<--contents of pair-table at 0x%x\n", (int) t);
  for (i = 0; i < t->size; i++) {
    PairHashTableEntry *e = &t->t[i];
    while (e) {
      if (e->p != -1)
	printf("(%d,%d,%d)[%d] ", e->p, e->q, e->n, i);
      e = e->overflow;
    }
  }
  printf("\n--->\n");
}
#endif
