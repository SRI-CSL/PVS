/*
 DOCUMENTATION INFORMATION				    module: HASH TABLE
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system	   : Apollo DN3000, HP 9000/S500, IBM RT 125/135
 file	   : hash.c
 unit-title: 
 ref.	   : 
 author(s) : Copyright (c) 1988-1997 G.L.J.M. Janssen
 date	   : 14-APR-1997
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>

#include "alloc.h"
#include "hash.h"
/* #include "bstring.h"*/

/* Define this to cause debugging code to be compiled: */
/* #define CC_DEBUG*/

#ifdef COMMENT
#define CALLOC_HASHTAB_ENTRY()		CALLOC_STRUCT (HASHTAB_ENTRY)
#define FREE_HASHTAB_ENTRY(h)		FREE (h)
#endif

#define CALLOC_HASHTAB_ENTRY() \
	( \
	 all_hashtab_entries ? (temp_hashtab_entry = all_hashtab_entries, \
           all_hashtab_entries = \
	      (HASHTAB_ENTRY_PTR) (all_hashtab_entries->info), \
	   *temp_hashtab_entry = null_hashtab_entry, \
	   temp_hashtab_entry) \
	 : CALLOC_STRUCT (HASHTAB_ENTRY) \
	)

#define FREE_HASHTAB_ENTRY(c) \
	{ \
	  HASHTAB_ENTRY_PTR _xyz_c = c; \
	  \
          _xyz_c->info = (void *) all_hashtab_entries; \
	  all_hashtab_entries = _xyz_c; \
	}

/* Some useful? local macros directly using entries array of hashtab. */
#define X_EMPTY_BUCKET(entries,h)	(!entries[h])
#define X_OCCUPIED_BUCKET(entries,h)	(entries[h])
#define X_KEYLEN(entries,h)		entries[h]->keylen
#define X_KEYSTR(entries,h)		entries[h]->keystr
#define X_KEYINFO(entries,h)		entries[h]->info
#ifdef USE_SHADOW
#define X_BACKLINK(entries,h)		entries[h]->backlink
#else
#define X_BACKLINK(entries,h)		h
#endif

HASHTAB_ENTRY_PTR all_hashtab_entries = NULL;
HASHTAB_ENTRY     null_hashtab_entry = {0};
HASHTAB_ENTRY_PTR temp_hashtab_entry;

int hash_copy_key = 1;

int hash_case_insensitive = 0;

/* A table of over 3 million entries should be enough for most applications
   we foresee now, so no checks are done on overflow. Feel free to extend
   when necessary by adding more primes.
*/
static int primes[] =
{
/* index:     ~ 12.5 * 2^index: */
/*  0 */      11,
/*  1 */      23,
/*  2 */      53,
/*  3 */     101,
/*  4 */     199,
/*  5 */     401,
/*  6 */     797,
/*  7 */    1601,
/*  8 */    3203,
/*  9 */    6397,
/* 10 */   12799,
/* 11 */   25601,
/* 12 */   51199,
/* 13 */  102397,
/* 14 */  204803,
/* 15 */  409597,
/* 16 */  819187,
/* 17 */ 1638431,
/* 18 */ 3276881
};

#define LAST_PRIMES_INDEX	(sizeof (primes) / sizeof (int) - 1)

static int nearest_primes_index (int size)
{
  int i;

  for (i = 0; i <= LAST_PRIMES_INDEX; i++)
    if (primes[i] >= size)
      return i;

  return LAST_PRIMES_INDEX;
}

int nearest_prime (int size)
{
  return primes[nearest_primes_index (size)];
}

/* When non-0 copies the key string into the hash table entry, else
   just holds pointer to it. */
static int copy_the_entry = 1;

/* Run-time statistics and messages. */
int RT_DEBUG = 0;

/* FOR DOCUMENTATION ON THE FUNCTIONS SEE HASH.H */
static void free_hashtab_entry (HASHTAB *tab, int index);

static int lookup_1 (HASHTAB *tab,		/* the table */
		     HASHTAB_ENTRY_PTR entry,	/* the entry to look for */
		     int *do_insert);		/* insert it? */

HASHTAB *make_hashtab (int index)
{
  HASHTAB *tab;
  int size = primes[index];

  tab = MALLOC_STRUCT (HASHTAB);

/* Current maximum size (i.e. number of entries) of hash table: */
  tab->size          = size;

/* This reflects the number of items currently held by the hash table: */
  tab->nr_items      = 0;

/* This counts the number of times an insert of an item is done, i.e.
   number of times `lookup (,,, INSERT)' is called. */
  tab->nr_inserts    = 0;

/* We count as collisions the fact that when an item has to be inserted
   the entry at its hash index is occupied by a different one. Subsequent
   tries to resolve this collision are not counted as new collisions.
   Merely looking up an item will never increase the collision count.
*/
  tab->nr_collisions = 0;

/* Number of rehashes undertaken for this table: */
  tab->nr_rehashes   = 0;

/* Current index in primes table: */
  tab->primes_index  = index;

#ifdef ALLOW_REHASH
/* Function called when non-NULL to process change of entry
   when rehashing in process.
*/
  tab->rehash_function = 0;
#ifdef USE_SHADOW
  {
    register int i;
    register int *p;

    tab->shadow_free_index = 0;
    tab->shadow_table = MALLOC_ARRAY (size, int);
    for (i = 0, p = tab->shadow_table; i < size; i++)
      *p++ = NOT_PRESENT;
  }
#endif
#endif

/* Allocate an array of empty entries: */
  tab->entries = CALLOC_ARRAY (size, HASHTAB_ENTRY_PTR);

  return tab;
}

HASHTAB *hashtab_create (int nr_items)
{
  return make_hashtab (nearest_primes_index (nr_items));
}

void reinit_hashtab (HASHTAB *tab)
{
  register int hashtab_size = tab->size;

  /* Free all entries allocated by hash table routines: */
  while (hashtab_size--)
    free_hashtab_entry (tab, hashtab_size);

  tab->nr_inserts    = 0;
  tab->nr_collisions = 0;
  tab->nr_rehashes   = 0;

#ifdef ALLOW_REHASH
  /* keep assigned rehash_function! */
#ifdef USE_SHADOW
  /* keep allocated shadow_table but do as if it is empty: */
  tab->shadow_free_index = 0;
#endif
#endif
}

static void print_symbol_info (FILE *fp, HASHTAB *symtab, int i)
{
  fprintf (fp, "%3d   0x%08x (%3d)  %s\n",
	   i,
	   KEYINFO (symtab, i),
	   KEYLEN  (symtab, i),
	   KEYSTR  (symtab, i));
}

void print_hashtab (FILE *fp, HASHTAB *tab)
{
  fprintf (fp, "Idx: Info (hex): Len:   Name:\n");
  FOR_EACH_OCCUPIED_BUCKET (tab, i) {
    print_symbol_info (fp, tab, i);
  } END_FOR_EACH_OCCUPIED_BUCKET;

  fprintf (fp, "Number of items: %d\n", tab->nr_items);
  fprintf (fp, "Inserts   total: %d\n", tab->nr_inserts);
  fprintf (fp, "Collision total: %d\n", tab->nr_collisions);
  fprintf (fp, "Nr. of rehashes: %d\n", tab->nr_rehashes);
}

/* This is the hash function. */
static int hash (int tabsize, const char *s, int len)
{
  register const char *p = s;
  register const char *end = p + len;
  register char c;
  register int hashval = 0;

  while (p != end) {
    c = *p++;
    if (c >= 0140) c -= 40;	/* 0140 = ' */
    hashval = ((hashval<<3) + (hashval>>28) + c);
  }
  /* Clearing MSB: */
  hashval &= INT_MAX;
  return (hashval < tabsize) ? hashval : hashval % tabsize;
}

/* Make tab bigger by a factor of approx. 2. */
static HASHTAB *hashtab_full_handler (HASHTAB *tab)
{
#ifdef ALLOW_REHASH
  int old_size = tab->size;
  /* Quick access to old array of entries: */
  HASHTAB_ENTRY_PTR *old_entries = tab->entries;
  register HASHTAB_ENTRY_PTR *entryp = old_entries;
  int newsize;
  register int i, newi;
  int insert_var;

  if (RT_DEBUG)
    print_message ("IHST002", "Rehashing (old size: %d)...\n", old_size);

  /* Use same HASHTAB struct for the new table; Pick next prime
     number, and get a fresh array of entries, initially all EMPTY:
  */
  newsize       = primes[++(tab->primes_index)];
  tab->entries  = CALLOC_ARRAY (newsize, HASHTAB_ENTRY_PTR);
  tab->size     = newsize;
  tab->nr_items = 0;
  tab->nr_rehashes++;

#ifdef USE_SHADOW
  {
    register int *p;

    tab->shadow_table = REALLOC_ARRAY (tab->shadow_table, newsize, int);
    /* tab->shadow_free_index = old_size;*/

    for (i = old_size, p = tab->shadow_table + old_size; i < newsize; i++)
      *p++ = NOT_PRESENT;
  }
#endif

  /* No need to copy entries while inserting here: */
  copy_the_entry = 0;

  /* The real re-hashing: */
  for (i = 0; i < /*old*/ old_size; i++, entryp++)
    if (*entryp) {
      /* Entry i is occupied; insert it in new table: */
      insert_var = (int) INSERT;
      /* lookup_1 returns index of this entry in tab: */
      newi = lookup_1 (tab, *entryp, &insert_var);
      /* Call user supplied function to account for change: */
      if (tab->rehash_function)
	(*(tab->rehash_function)) (i, newi);
    } /*if-for*/

  /* No longer need entries-array of old table: */
  MA_FREE_ARRAY (old_entries, old_size, HASHTAB_ENTRY_PTR);
  copy_the_entry = 1;

  if (RT_DEBUG) {
    print_message ("IHST003", "Rehashing (new size: %d)...done.\n", newsize);
    print_hashtab (stderr, tab);
  }

  return tab;
#else
  print_message ("EHST001", "Hashtable full; program aborted.\n");

#ifdef CC_DEBUG
  print_hashtab (tab);
#endif

  exit (1);
#endif
}

static int lookup_1 (HASHTAB *tab,		/* the table */
		     HASHTAB_ENTRY_PTR entry,	/* the entry to look for */
		     int *do_insert)		/* insert it? */
{
  HASHTAB_ENTRY_PTR *entries;
  int hashtab_size;
  register int d;
  register int h;
  register char *s = entry->keystr;
  register int len = entry->keylen;
  int first_time   = 1;
  int insert_flag  = *do_insert;

  if (!tab) {
    *do_insert = NOT_PRESENT;
    return NOT_PRESENT;
  }

  if (insert_flag) tab->nr_inserts++;

 restart:
  entries      = tab->entries;
  hashtab_size = tab->size;

  h = hash (hashtab_size, s, len);
  d = 1;

  while (1) {
    if (X_EMPTY_BUCKET (entries, h)) {
      if (insert_flag) {

	tab->nr_items++;

	if (copy_the_entry) {	/* user initiated insert */
	  char *p;

	  entries[h] = CALLOC_HASHTAB_ENTRY ();
	  if (hash_copy_key) {
	    /* CALLOC_STRING includes terminating null char! */
	    p = X_KEYSTR (entries, h) = CALLOC_STRING (len);
	    memcpy (p, s, len);
	  }
	  else
	    X_KEYSTR (entries, h) = s;
	  /* p[len] = '\0'; CALLOC_STRING zeroes */
	  X_KEYLEN  (entries, h) = len;
	  X_KEYINFO (entries, h) = entry->info;

#ifdef ALLOW_REHASH
#ifdef USE_SHADOW
	  tab->shadow_table[tab->shadow_free_index] = h;
	  X_BACKLINK (entries, h) = tab->shadow_free_index++;
#endif
#endif
	}
	else {			/* rehash initiated insert */
	  entries[h] = entry;

#ifdef ALLOW_REHASH
#ifdef USE_SHADOW
	  /* Update shadow table: */
	  tab->shadow_table[X_BACKLINK (entries, h)] = h;
#endif
#endif
	}
	*do_insert = INDEED_INSERTED;
	return h;
      }
      /* In case of LOOKUP make sure NOT to return ALREADY_PRESENT: */
      *do_insert = NOT_PRESENT;
      return NOT_PRESENT;
    }

    /* Here: X_OCCUPIED_BUCKET (h) */
    if (   len == X_KEYLEN (entries, h)
	&& (   s == X_KEYSTR (entries, h)
	    || (hash_case_insensitive ?
		  !strncasecmp (s, X_KEYSTR (entries, h), len)
		: !memcmp (s, X_KEYSTR (entries, h), len)))) {

      *do_insert  = ALREADY_PRESENT;
      entry->info = X_KEYINFO (entries, h);
      return h;
    }

    /* Here: COLLISION */
    if (insert_flag && first_time) {
      tab->nr_collisions++;
      first_time--;
    }

    if (d == hashtab_size) {
      if (!insert_flag) {
	/* In case of LOOKUP make sure NOT to return ALREADY_PRESENT: */
	*do_insert = NOT_PRESENT;
	return NOT_PRESENT;
      }
      tab = hashtab_full_handler (tab);
      goto restart;
    }

    /* Quadratic probing to find free place: */
    h += d;
    d += 2;
    if (h >= hashtab_size)
      h -= hashtab_size;	     
  } /*while (1)*/
}

int lookup (HASHTAB *tab, const char *s, int len, void **info, int *do_insert)
{
  HASHTAB_ENTRY entry;
  int insert_var;
  int report_back = 0;
  int index;

  /* Prepare entry for lookup_1: */
  entry.keystr = (char *) s;
  entry.keylen = len;
  entry.info   = info ? *info : NULL;

  if (do_insert == LOOKUP || do_insert == INSERT)
    /* No reporting back possible. */
    insert_var = (int) do_insert;
  else {
    /* Assume do_insert holds address of int variable. */
    insert_var = *do_insert;
    report_back++;
  }

  index = lookup_1 (tab, &entry, &insert_var);
  if (info) *info = entry.info;
  if (report_back) *do_insert = insert_var;

#ifdef ALLOW_REHASH
#ifdef USE_SHADOW
  if (index >= 0)
    index = X_BACKLINK (tab->entries, index);
#endif
#endif

  return index;
}

/* Frees the entry at `index' in hash table `tab'.
   (must be address of HASHTAB variable).
   Also frees the key string but does not affect the info field.
   Afterwards EMPTY_BUCKET (tab.entries, index) is true.
*/
static void free_hashtab_entry (HASHTAB *tab, int index)
{
  register HASHTAB_ENTRY_PTR *entries = tab->entries;

  if (OCCUPIED_BUCKET (tab, index)) {
    index = SHADOW (tab, index);

    tab->nr_items--;
    MA_FREE_BYTES (X_KEYSTR (entries, index), X_KEYLEN (entries, index) + 1);

#ifdef ALLOW_REHASH
#ifdef USE_SHADOW
    tab->shadow_table[X_BACKLINK (entries, index)] = NOT_PRESENT;
#endif
#endif

    FREE_HASHTAB_ENTRY (entries[index]);
    tab->entries[index] = (HASHTAB_ENTRY_PTR) 0;
  }
}

void free_hashtab (HASHTAB *tab)
{
  register int hashtab_size = tab->size;

  while (hashtab_size--)
    free_hashtab_entry (tab, hashtab_size);

  MA_FREE_ARRAY (tab->entries, tab->size, HASHTAB_ENTRY_PTR);

#ifdef ALLOW_REHASH
#ifdef USE_SHADOW
  MA_FREE_ARRAY (tab->shadow_table, tab->size, int);
#endif
#endif
  MA_FREE_STRUCT (tab, HASHTAB);
}

/* Change this string if you prefer other prefixes: */
static const char *prefix = "X_";
static int gen_counter = 0;

int gen_unique_entry (HASHTAB *tab)
{
  char buf[32], *startp;
  register int i, len = strlen (prefix);
  int inserted;

  strcpy (buf, prefix);
  startp = &buf[len];

  do {
    sprintf (startp, "%d", gen_counter++);
    len = strlen (buf);
    inserted = (int) INSERT;
    i = lookup (tab, buf, len, NULL, &inserted);
  } while (inserted == ALREADY_PRESENT);

  /* Here: inserted == INDEED_INSERTED, so i is the right index. */
  return i;
}

void do_hashtab (HASHTAB *tab,
		 int (* function) (HASHTAB *, int, int),
		 int misc)
{
  register int i;
  int hashtab_size;
#ifdef USE_SHADOW
  register int *p = tab->shadow_table;

  hashtab_size = tab->shadow_free_index;

  for (i = 0; i < hashtab_size; i++)
    if (*p++ != NOT_PRESENT)
      (* function) (tab, i, misc);
#else
  register HASHTAB_ENTRY_PTR *entries = tab->entries;

  hashtab_size = tab->size;

  for (i = 0; i < hashtab_size; i++)
    if (*entries++)
      (* function) (tab, i, misc);
#endif
}

#ifdef CC_DEBUG

/* The function that is called when rehashing hashtab: */
void var_table_rehash (int oldi, int newi)
{
  printf ("Moving entry at index %d to index %d.\n", oldi, newi);
}

main (int argc, char* argv[])
{
  char s[256];
  int lc = 0;
  int i;
  HASHTAB *hashtab;
  HASHTAB *hashtab2;

  RT_DEBUG = 1;

  hashtab  = make_hashtab (0);
  SET_REHASH_FUNCTION (hashtab, var_table_rehash);

  hashtab2 = make_hashtab (5);

/*
  i = gen_unique_entry (hashtab);
  free_hashtab_entry (hashtab, i);
  i = gen_unique_entry (hashtab);
  free_hashtab_entry (hashtab, i);
  i = gen_unique_entry (hashtab);
  free_hashtab_entry (hashtab, i);
*/
  reinit_hashtab (hashtab);

  while (gets (s)) {
    lc++;
    if (lc & 1)
      lookup (hashtab , s, strlen (s), NULL, INSERT);
    else
      lookup (hashtab2, s, strlen (s), NULL, INSERT);
  }
  print_hashtab (hashtab);
  print_hashtab (hashtab2);

  printf ("Nr. input lines: %d\n", lc);

  free_hashtab (hashtab2);
  free_hashtab (hashtab);
}
#endif
