/* hash.h */
/* hash table routines based on David Long's Bdd package */

/*
 * MONA is Copyright (C) 1997 BRICS. All rights reserved. 
 *
 * Reproduction of all or part of this software is permitted for
 * educational or research use on condition that this copyright notice is
 * included in any copy. This software comes with no warranty of any
 * kind. In no event will BRICS be liable for any damages resulting from
 * use of this software.
 */

#ifndef __HASH_H
#define __HASH_H

#include "long_mem/memuser.h"

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

extern void insert_in_hash_tab(hash_tab, long, long, pointer);
extern pointer lookup_in_hash_tab(hash_tab, long, long) ;
extern hash_tab new_hash_tab(long (*hash_fn)(long, long), 
                             char (*eq_fn)(long, long, long, long));
extern void free_hash_tab(hash_tab);
extern long hash2(long, long);
extern char eq2(long, long, long, long);
extern long hashlong(long, long);
extern char eqlong(long, long, long, long);
extern void free_hash_tab_with_destructor(hash_tab, void (*destruct)(long,long,pointer));

#endif
