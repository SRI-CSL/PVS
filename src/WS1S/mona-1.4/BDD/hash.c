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

#include "../Mem/mem.h"
#include "hash.h"

long primes[]=
{
  1,
  2,
  3,
  7,
  13,
  23,
  59,
  113,
  241,
  503,
  1019,
  2039,
  4091,
  8179,
  11587,
  16369,
  23143,
  32749,
  46349,
  65521,
  92683,
  131063,
  185363,
  262139,
  330287,
  416147,
  524269,
  660557,
  832253,
  1048571,
  1321109,
  1664501,
  2097143,
  2642201,
  3328979,
  4194287,
  5284393,
  6657919,
  8388593,
  10568797,
  13315831,
  16777199,
  33554393,
  67108859,
  134217689,
  268435399,
  536870879,
  1073741789,
  2147483629
};

#define TABLE_SIZE(size_index) (primes[size_index])
#define REDUCE(i, size)\
do\
  {\
    (i)%=(size);\
    if ((i) < 0)\
      (i)= -(i);\
  }\
while (0)

long hash2(long d1, long d2)
{ 
  return(d1*10007+d2);
}

char eq2(long d11, long d12, long d21, long d22)
{
  return((d11 == d21) && (d12 == d22) );
}

long hashlong(long p1, long p2)
{  long res = 0;

  while(*(int *)p1 != -1) res = ((res*100001) + ((*((int *)p1))++));
   return(res);
}   

char eqlong(long p11, long p12, long p21, long p22)
{
   while((*((int *)p11)!= -1) && (*((int *)p11) == *((int *)p21)))
     {(*((int *)p11))++ ; (*((int *)p21))++;}
   if((*((int *)p11) != -1) || (*((int *)p21) != -1)) return (0); 
   else return (1);
} 


   
/* rehash_hash_tab(h) increases the size of h by roughly a */
/* factor of 2 and rehashes all of its entries. */

static void rehash_hash_tab(hash_tab h)
{
  long i;
  long hash;
  long oldsize;
  hash_rc *newtable;
  hash_rc p, q;

  oldsize=h->size;
  h->size_index++;
  h->size=TABLE_SIZE(h->size_index);
  newtable=(hash_rc *)mem_alloc((size_t)(h->size*sizeof(hash_rc)));
  for (i=0; i < h->size; ++i)
    newtable[i]=0;
  for (i=0; i < oldsize; ++i)
    for (p=h->table[i]; p; p=q)
      {
	q=p->next;
	hash=h->hash_fn(p->key1, p->key2);
	REDUCE(hash, h->size);
	p->next=newtable[hash];
	newtable[hash]=p;
      }
  mem_free((void *)h->table);
  h->table=newtable;
}


/* insert_in_hash_tab(h, f, data) associates the specified data */
/* with f in h. */

void
insert_in_hash_tab(hash_tab h, long f, long g, void * data)
{
  long hash;
  hash_rc p;

  p=mem_alloc(sizeof(*p));
  p->key1=f;
  p->key2=g;
  p->data=data;
  hash=h->hash_fn(f,g);
  REDUCE(hash, h->size);
  p->next=h->table[hash];
  h->table[hash]=p;
  h->entries++;
  if ((h->size << 2) < h->entries)
    rehash_hash_tab(h);
}


/* bdd_lookup_in_hash_tab(h, f) looks up f in h and returns either a */
/* pointer to the associated data or null. */

void *
lookup_in_hash_tab(hash_tab h, long f, long g)
{
  long hash;
  hash_rc p;

  hash=h->hash_fn(f,g);
  REDUCE(hash, h->size);
  for (p=h->table[hash]; p; p=p->next)
    if (h->eq_fn(p->key1, p->key2, f, g))
      return (p->data);
  return ((void *)0);
}


/* new_hash_tab(item_size) creates a new hash table with */
/* the key a sequence of longs. */

hash_tab
new_hash_tab(long (*hash_fn)(long, long), 
             char (*eq_fn)(long, long, long, long))
{
  long i;
  hash_tab h;

  h=(hash_tab)mem_alloc(sizeof(struct hash_tab_));
  h->size_index=6;
  h->size=TABLE_SIZE(h->size_index);
  h->table=(hash_rc *)mem_alloc((size_t)(h->size*sizeof(hash_rc)));
  for (i=0; i < h->size; ++i)
    h->table[i]=0;
  h->entries=0;
  h->hash_fn = hash_fn;
  h->eq_fn = eq_fn;
  return (h);
}


/* free_hash_tab(h) frees up the storage associated with h. */

void
free_hash_tab(hash_tab h)
{
  long i;
  hash_rc p, q;

  for (i=0; i < h->size; ++i)
    for (p=h->table[i]; p; p=q)
      {
	q=p->next;
	mem_free((void *)p);
      }
  mem_free((void *)h->table);
  mem_free(h);
}

