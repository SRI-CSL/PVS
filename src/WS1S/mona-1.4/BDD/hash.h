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

/* hash table routines based on David Long's BDD package */

#ifndef __HASH_H
#define __HASH_H

struct hash_rc_
{
  long key1;
  long key2;
  void * data;
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

extern void insert_in_hash_tab(hash_tab, long, long, void *);
extern void *lookup_in_hash_tab(hash_tab, long, long) ;
extern hash_tab new_hash_tab(long (*hash_fn)(long, long), 
                             char (*eq_fn)(long, long, long, long));
extern void free_hash_tab(hash_tab);
extern long hash2(long, long);
extern char eq2(long, long, long, long);
extern long hashlong(long, long);
extern char eqlong(long, long, long, long);
extern void free_hash_tab_with_destructor(hash_tab, void (*destruct)(long,long,void *));

#endif
