/*
 DOCUMENTATION INFORMATION				    module: HASH TABLE
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system	   : Apollo DN3000, HP 9000/S500, IBM RT 125/135
 file	   : hash.h
 unit-title: 
 ref.	   : 
 author(s) : Copyright (c) 1988-1997 G.L.J.M. Janssen
 date	   : 14-APR-1997
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

#ifndef HASHTAB_H
#define HASHTAB_H

#include <stdio.h>

/* Include file for hashtable module. */
/* Declares all names needed to use it. */

/* Here a hashtable is implemented as a C struct with a dynamically
   allocated array of entries. In fact, the entries array is an array
   of pointers to the real entries. A real entry again is a struct with
   3 fields: the key string which is a character pointer, the length of
   the key string and a general purpose void pointer info field.
   The key string and key length data are used by some of the hashtable
   routines; the info field's meaning is entirely the user's responsibility.
   In most cases it will be the hook to user defined data associated with
   this hashtable entry.

   When using shadowing the hash table will always be accessed through an
   additional indirection table which allows for smooth rehashing such that
   for the application this rehashing is transparant.
*/
   
/* Define this to allow rehashing upon table full, else an error
   message is printed on stderr and the program is aborted.
   Default table size is 11 entries, and is about doubled on each rehash.
   See the struct HASHTAB for info on keeping track of repositioned entries.
*/
#define ALLOW_REHASH

/* Define this to allow for automatically use of a shadow table of indices.
   This shadow table indirects lookups by always going through it to the
   real hash table.
   This is subordinate to ALLOW_REHASH, i.e. to be effective ALLOW_REHASH
   must also be defined. Upon rehash the shadow table will be properly extended
   and updated. The start of the new area will be marked by shadow_free_index.
*/
#define USE_SHADOW

typedef struct HASHTAB_ENTRY {
  int	keylen;			/* length of keystr field */
  char *keystr;			/* the string key */
  void *info;			/* general field for extra info */
#ifdef ALLOW_REHASH
#ifdef USE_SHADOW
  int backlink;			/* gives corr. index in shadow_table. */
#endif
#endif
} HASHTAB_ENTRY, *HASHTAB_ENTRY_PTR;

/* Note: a hash table is a C struct. Its entries field is an (dynamic) array
   of pointers to entry structs.
*/
typedef struct HASHTAB {
  int size;			/* size of entries array */
  int nr_items;			/* nr. items contained = occupied entries */
  int nr_inserts;		/* nr. attempted inserts */
  int nr_collisions;		/* nr. collisions */
  int nr_rehashes;		/* nr. rehashes */
  int primes_index;		/* curr. index in primes[] table */
#ifdef ALLOW_REHASH
  void (*rehash_function) ();	/* function called when non-NULL */
				/* to process change of entry */
				/* when rehashing in process. */
				/* Rehashing might happen upon lookup with */
  				/* INSERT when the table is full. */
				/* Will be called as: */
  /* rehash_function (old_index, new_index); */
				/* i.e. you get the old index value of the */
				/* entry and the index where it is put */
                                /* in the rehashed table. You may */
				/* still refer to the info by looking at */
  				/* old_index in the original table. */
#ifdef USE_SHADOW
  int shadow_free_index;	/* Index in shadow table where new */
                                /* indirections are stored when items are */
				/* added to the hash table. */
  int *shadow_table;		/* A shadow table. Maps old indices before */
				/* the rehash to the new ones. Lookups will */
				/* automatically use this table. */
#endif
#endif
  HASHTAB_ENTRY_PTR *entries;
} HASHTAB;

#define HASHTAB_SIZE(tab)	((tab) ? (tab)->size : 0)

/* Useful symbols for values of do_insert in lookup (). */
#define LOOKUP			((int *)0)
#define INSERT			((int *)1)

/* Symbols for values returned by do_insert in lookup (). */
#define ALREADY_PRESENT		0
#define INDEED_INSERTED		1

/* Returned by lookup when string not present in table upon LOOKUP: */
#define NOT_PRESENT		(-1)

/* Useful user accessible macros: */
#ifdef USE_SHADOW
#define SHADOW(tab, h)		((tab)->shadow_table[h])
#define EMPTY_BUCKET(tab,h)    	(SHADOW (tab, h) == NOT_PRESENT)
#define OCCUPIED_BUCKET(tab,h)	(SHADOW (tab, h) != NOT_PRESENT)
#else
#define SHADOW(tab, h)		h
#define EMPTY_BUCKET(tab,h)    !(tab)->entries[h]
#define OCCUPIED_BUCKET(tab,h)	(tab)->entries[h]
#endif

#define KEYLEN(tab,h)		((tab)->entries[SHADOW (tab, h)]->keylen)
#define KEYSTR(tab,h)		((tab)->entries[SHADOW (tab, h)]->keystr)
#define KEYINFO(tab,h)		((tab)->entries[SHADOW (tab, h)]->info)

#define SET_REHASH_FUNCTION(tab,func)	(tab)->rehash_function = func

#ifdef USE_SHADOW
#define FOR_EACH_OCCUPIED_BUCKET(hashtab, i) \
	{ \
	  register int i; \
          int _hashtab_size = (hashtab) ? (hashtab)->shadow_free_index : 0; \
	\
	  for (i = 0; i < _hashtab_size; i++) \
            if (OCCUPIED_BUCKET (hashtab, i)) {
#else
#define FOR_EACH_OCCUPIED_BUCKET(hashtab, i) \
	{ \
	  register int i; \
          int _hashtab_size = HASHTAB_SIZE (hashtab); \
	\
	  for (i = 0; i < _hashtab_size; i++) \
            if (OCCUPIED_BUCKET (hashtab, i)) {
#endif

#define END_FOR_EACH_OCCUPIED_BUCKET	}}

extern int nearest_prime (int size);

/* Set to 1 by default. Causes the key string always to be copied into the
   hash table entry. When turned off (= 0), no copies are made.
   This is useful to make all key strings canonical.
*/
extern int hash_copy_key;

/* Set to 0 by default. Causes the key string to be literally compared against
   strings stored in the table.
   When turned on (!= 0), strings are compared using `strncasecmp'.
*/
extern int hash_case_insensitive;

/* When set, causes some informative messages to be send to stderr. */
extern int RT_DEBUG; /* = 0 */

/* Returns a pointer to a new hash table,
   ready to hold at least nr_items entries.
*/
extern HASHTAB *hashtab_create (int nr_items);

/* Returns a pointer to a new hash table,
   ready to hold about 12.5 * (2 ^ index) entries.
   Min. value for index is 0;
   Max. value for index is currently 13.
*/
extern HASHTAB *make_hashtab (int index);

/* Looks up string `s' of length `len' in hash table `tab'
   String need not be null terminated and may contain null characters.
   Returns index >= 0 if found else `NOT_PRESENT'.

   `do_insert' can be one of the constants LOOKUP or INSERT, or the
   address of an int variable containing one of those constants.
   LOOKUP means: just see if `s' is already present.
   INSERT means: insert the string when not already present.
                 String is copied in this case and a null terminator character
                 is always added (and room allocated for).
   In case `do_insert' holds the address of an int variable, ALREADY_PRESENT
   or INDEED_INSERTED is reported back through it.

   When a new item is inserted it is associated with the `info', which
   must be the address of a character pointer variable upon entry, or NULL
   if info is of no concern.
   When an item is retrieved, i.e. it was already present, its info is returned
   through `info'.

   Uses quadratic probing to resolve collisions. Unfortunately this might lead
   to a situation where the free entries are present but cannot be found by
   this probing scheme.
   Calls `hashtab_full_handler' upon full hash table.
*/

extern int lookup(HASHTAB *tab, const char *s, int len, void **info, int *do_insert);

/* Frees all space allocated for hash tab. */
extern void free_hashtab (HASHTAB *tab);

/* Does the minimum number of actions to make `tab' again a fresh
   empty table of its current size. Also any assigned rehash function
   is kept.
*/
extern void reinit_hashtab (HASHTAB *tab);

/* Generates a entry in the hash table `tab' (must be address of HASHTAB
   variable) with a name that is unique w.r.t. the names already contained
   in the hash table.

   Names generated look like "X_<number>".
   Inserts the new name, clears the info field and returns the hash index.
*/
extern int gen_unique_entry (HASHTAB *tab);

/* Calls `function (tab, i, misc)' for every occupied entry `i' in
   the hashtable `tab'.
*/
extern void do_hashtab (HASHTAB *tab,
			int (* function) (HASHTAB *, int, int),
			int misc);

/**/
extern void print_hashtab (FILE *fp, HASHTAB *tab);

#endif /* HASHTAB_H */
