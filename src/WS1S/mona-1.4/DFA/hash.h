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
