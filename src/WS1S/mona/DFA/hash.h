/* hash.h */

/*
 * MONA is Copyright (C) 1997 BRICS. All rights reserved. 
 *
 * Reproduction of all or part of this software is permitted for
 * educational or research use on condition that this copyright notice is
 * included in any copy. This software comes with no warranty of any
 * kind. In no event will BRICS be liable for any damages resulting from
 * use of this software.
 */

#ifndef _HASH_TABS
#define _HASH_TABS

struct hash_rc_
{
  long key1;
  long key2;
  pointer data;
  struct hash_rc_ *next;
};

typedef struct hash_rc_ *hash_rc;

struct hash_tab_
{
  hash_rc *table;
  int size_index;
  long size;
  long entries;
  long (*hash_fn)(long, long);
  char (*eq_fn)(long, long, long, long);
};

typedef struct hash_tab_ *hash_tab;


/* Hash table routines */

void insert_in_hash_tab(hash_tab, long, long, pointer);
pointer lookup_in_hash_tab(hash_tab, long, long) ;
hash_tab new_hash_tab(long (*hash_fn)(long, long), 
                             char (*eq_fn)(long, long, long, long));
void free_hash_tab(hash_tab);
long hash2(long, long);
char eq2(long, long, long, long);
long hashlong(long, long);
char eqlong(long, long, long, long);

#endif
