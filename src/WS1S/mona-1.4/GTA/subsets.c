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

#include <stdlib.h>
#include <stdio.h> /* for ssDump */
#include <string.h>
#include "../Mem/mem.h"
#include "subsets.h"

extern long primes[]; /* defined in ../BDD/hash.c */

unsigned ssHash(unsigned *set, unsigned len, unsigned size)
{
  unsigned hash = 0;
  while (len--) 
    hash = (hash << 1) + *set++ + 42; 
  return hash % size;
}

void ssInit(Subsets *s, unsigned singletons, unsigned initialCapacity)
{
  int i;
  s->prime = initialCapacity;
  s->size = primes[s->prime];
  s->overflows = 0;
  s->inverse = 0;
  s->inverseAllocated = 0;
  s->num = 0;
  s->singletons = singletons;
  s->t = (SubsetsEntry *) mem_alloc(sizeof(SubsetsEntry)*s->size);
  for (i = 0; i < s->size; i++) {
    s->t[i].length = 0; /* free mark */
    s->t[i].overflow = 0;
  }
}

void ssFree(Subsets *s)
{
  int i;
  for (i = 0; i < s->size; i++) {
    SubsetsEntry *e = s->t[i].overflow, *w;
    while (e) {
      w = e->overflow;
      mem_free(e->elements);
      invariant(s->inverse[e->n - s->singletons] = e);
      mem_free(e);
      e = w;
    }
    if (s->t[i].length != 0)
      mem_free(s->t[i].elements);
  }
  mem_free(s->t);
  mem_free(s->inverse);
}

int ssLookupAndInsert(Subsets *s, unsigned c1, unsigned c2, unsigned *n)
{
  unsigned t, len = 0, i1 = 0, i2 = 0;
  unsigned *unionSet;
  SubsetsEntry *e, *ee, *eee;
  SubsetsEntry *e1, *e2;

  if (c1 == c2) {
    /* same set, trivial union */
    *n = c1;
    return 1;
  }

  /* ensure that c1<c2 */
  if (c2 < c1) {
    t = c1;
    c1 = c2;
    c2 = t;
  }

  /* MAKE UNION SET */

  if (c2 < s->singletons) {
    /* both c1 and c2 are singletons */
    unionSet = (unsigned *) mem_alloc(sizeof(unsigned)*2);
    unionSet[len++] = c1;
    unionSet[len++] = c2;
  }
  else if (c1 >= s->singletons) {
    /* both c1 and c2 are non-singletons, merge the element arrays */
    e1 = s->inverse[c1 - s->singletons];
    e2 = s->inverse[c2 - s->singletons];
    unionSet = 
      (unsigned *) mem_alloc(sizeof(unsigned)*(e1->length + e2->length));

    while (i1 < e1->length && i2 < e2->length)
      if (e1->elements[i1] < e2->elements[i2])
	unionSet[len++] = e1->elements[i1++];
      else if (e1->elements[i1] >= e2->elements[i2]) {
	unionSet[len++] = e2->elements[i2++];
	if (e1->elements[i1] == e2->elements[i2-1])
	  i1++;
      }
    while (i1 < e1->length)
      unionSet[len++] = e1->elements[i1++];
    while (i2 < e2->length)
      unionSet[len++] = e2->elements[i2++];
  }
  else {
    /* c1 is singleton, c2 is non-singleton */
    e2 = s->inverse[c2 - s->singletons];
    unionSet = (unsigned *) mem_alloc(sizeof(unsigned)*(e2->length+1));

    while (i2 < e2->length && c1 > e2->elements[i2])
      unionSet[len++] = e2->elements[i2++];
    if (i2 == e2->length || c1 != e2->elements[i2])
      unionSet[len++] = c1;
    while (i2 < e2->length)
      unionSet[len++] = e2->elements[i2++];
  }

  /* SEARCH FOR THE UNION SET */

  e = ee = &s->t[ssHash(unionSet, len, s->size)];
  do {
    if (e->length == len && 
	memcmp(e->elements, unionSet, sizeof(unsigned)*len) == 0) {

      /* found it */
      *n = e->n;
      mem_free(unionSet);
      return 1;
    }
    eee = e;
    e = e->overflow;
  } while (e);

  /* NOT FOUND, INSERT IT */

  if (ee->length != 0) { /* main entry occupied? */
    ee = (SubsetsEntry *) mem_alloc(sizeof(SubsetsEntry));
    eee->overflow = ee;
    s->overflows++;
  } /* ee is now the new entry */

  /* insert into inverse table */
  if (s->num == s->inverseAllocated) {
    s->inverseAllocated = s->inverseAllocated*2 + 5;
    s->inverse = (SubsetsEntry **) 
      mem_resize(s->inverse, sizeof(SubsetsEntry *)*s->inverseAllocated);
  }
  s->inverse[s->num] = ee;

  /* assign it a number (successively) */
  *n =  s->singletons + s->num++;

  /* fill entry */
  ee->elements = unionSet;
  ee->length = len;
  ee->n = *n;
  ee->c1 = c1;
  ee->c2 = c2;
  ee->overflow = 0;
  
  if (s->overflows > s->size*2) { 

    /* TOO MANY OVERFLOWS, REHASH */
    unsigned newsize = primes[++(s->prime)];
    SubsetsEntry *r = 
      (SubsetsEntry *) mem_alloc(sizeof(SubsetsEntry)*newsize);
    int i;
    s->overflows = 0;

    /* clear new array */
    for (i = 0; i < newsize; i++) { 
      r[i].length = 0;
      r[i].overflow = 0;
    }
    /* rehash */
    for (i = 0; i < s->size; i++) { 
      SubsetsEntry *w = &s->t[i], *ww;
      if (w->length != 0)
	while (w) {
	  SubsetsEntry *d = &r[ssHash(w->elements, w->length, newsize)];
	 
	  /* find back of list */
	  if (d->length != 0) { 
	    while (d->overflow)
	      d = d->overflow;
	    d->overflow = 
	      (SubsetsEntry *) mem_alloc(sizeof(SubsetsEntry));
	    d = d->overflow;
	    s->overflows++;
	  }
	  d->elements = w->elements;
	  d->length = w->length;
	  d->n = w->n;
	  d->c1 = w->c1;
	  d->c2 = w->c2;
	  d->overflow = 0;
	  s->inverse[d->n - s->singletons] = d;
	  ww = w;
	  w = w->overflow;
	  if (ww != &s->t[i])
	    mem_free(ww);
	}
    }
    mem_free(s->t);
    s->t = r;
    s->size = newsize;
  }

  return 0;
}

unsigned ssSize(Subsets *s)
{
  return s->singletons + s->num;
}

void ssGetComponents(Subsets *s, unsigned n, unsigned *c1, unsigned *c2)
{
  SubsetsEntry *e = s->inverse[n - s->singletons];

  *c1 = e->c1;
  *c2 = e->c2;
}

#ifndef NDEBUG
void ssDump(Subsets *s)
{
  int i, j;
  printf("DUMP:\n");
  for (i = 0; i < s->num; i++) {
    SubsetsEntry *e = s->inverse[i];
    
    printf("%i = {", i + s->singletons);
    for (j = 0; j < e->length; j++)
      printf("%s%i", j==0 ? "" : ", ", e->elements[j]);
    printf("}\n");
  }
}
#endif
