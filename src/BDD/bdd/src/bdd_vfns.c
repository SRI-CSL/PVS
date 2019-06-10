/*
 DOCUMENTATION INFORMATION				         module: BDD
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system	   : Apollo DN3000, HP 9000/S500, IBM RS/6000
 file	   : bdd_vfns.c
 unit-title: 
 ref.	   : Efficient Implementation of a BDD Package, Karl S. Brace. DAC'90
 author(s) : Copyright (c) 1990-1998 G.L.J.M. Janssen
 date	   : 29-JAN-1998
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

/* This code is in an experimental state and therefore subject to
   possible severe changes and modifications.
   Most of the functionality will however also be supported in future versions
   although probably in a different form, i.e., using different external
   names for global variables and routines.
*/

#include <stdio.h>
#include <stdlib.h>

#include "alloc.h"
#include "bdd_fns.h"
#include "bdd_vfns.h"

/* Vectors of BDD's are represented by a C array of BDDPTR elements:
   BDDPTR *F = MakeBDDVec (size).
   An array of BDDPTRs is allocated of length `size + 1'.
   The return value is a pointer to the second element!
   The first (0 index element) is used to store the `size'.
   Use BDDVEC_SIZE (F).
   By definition F[0] is considered to refer to the MOST significant
   bit. In the comments a BDD vector is denoted using square brackets, like
   [ f1, f2, ..., fn ] for an n-bit vector.
*/
BDDPTR *MakeBDDVec (int size)
{
  if (size > 0) {
    BDDPTR *F;

    F = CALLOC_ARRAY (size+1, BDDPTR);
    F[0] = (BDDPTR) (long) size;
    return F+1;
  }
  return NULL;
}

void FreeBDDVec2 (register BDDPTR *F, register int size)
{
  if (F && size > 0)
    do {
      register BDDPTR f;

      f = *F++;
      bdd_free (f);
    } while (--size);
}

void FreeBDDVec (BDDPTR *F)
{
  register int size = BDDVEC_SIZE (F);

  if (F && size > 0) {
    FreeBDDVec2 (F, size);
    free (F-1);
  }
}

BDDPTR *CopyBDDVec2 (BDDPTR *F, register int size,
		     BDDPTR *toF)		/* space to put copy */
{
  register BDDPTR *p;

  p = toF;
  do
    *p++ = bdd_assign (*F++);
  while (--size);
  return toF;
}

BDDPTR *CopyBDDVec (BDDPTR *F)
{
  int size = BDDVEC_SIZE (F);

  if (size > 0)
    return CopyBDDVec2 (F, size, MakeBDDVec (size));
  return NULL;
}

void ComplBDDVec2 (register BDDPTR *F, register int size)
{
  do {
    register BDDPTR f = *F;

    *F++ = bdd_not (f);
    bdd_free (f);
  } while (--size);
}

void ComplBDDVec (BDDPTR *F)
{
  int size = BDDVEC_SIZE (F);

  if (size > 0)
    ComplBDDVec2 (F, size);
}

#if 0
/* Code is obsolete: improved and moved to bdd.c */

/* Must be prime number. With 797, spending ~10kbytes on cache. */
#define CONSTRAIN_CACHE_SIZE	797

typedef struct constrain_cache_entry CONSTRAIN_CACHE_ENTRY;

/* Per entry: 3 * sizeof (BDDPTR) = 3 * 4 = 12 bytes. */
static struct constrain_cache_entry {
  BDDPTR f, c, R;
} constrain_cache[CONSTRAIN_CACHE_SIZE] = {0};

static int nr_hits = 0;
static int nr_lookups = 0;
static int nr_collisions = 0;
static int occupancy = 0;

#define hash_constrain(a, b) \
  (((((int) a) ^ ((int) b << 5)) & INT_MAX) % CONSTRAIN_CACHE_SIZE)

static BDDPTR lookup_constrain_cache (BDDPTR f, BDDPTR c)
{
  BDDPTR R;
  CONSTRAIN_CACHE_ENTRY *entry;

  entry = constrain_cache + hash_constrain (f, c);

  nr_lookups++;

  if (   !BDD_VOID_P (R = entry->R)
      && BDD_EQUAL_P (f, entry->f)
      && BDD_EQUAL_P (c, entry->c)) {
    nr_hits++;
    return R;
  }
  return BDD_VOID;
}

static BDDPTR insert_constrain_cache (BDDPTR f, BDDPTR c, BDDPTR R)
{
  CONSTRAIN_CACHE_ENTRY *entry;

  entry = constrain_cache + hash_constrain (f, c);

  /* Let's be honest: check for equal data: */
  if (   !BDD_VOID_P (entry->R)
      && BDD_EQUAL_P (f, entry->f)
      && BDD_EQUAL_P (c, entry->c))
    /* Already stored. */
    return R;

  bdd_assign (f);
  bdd_assign (c);
  bdd_assign (R);
  if (!BDD_VOID_P (entry->R)) {
    nr_collisions++;
    bdd_free (entry->R);
    bdd_free (entry->f);
    bdd_free (entry->c);
  }
  else
    /* A fresh entry: */
    occupancy++;
  entry->f = f;
  entry->c = c;
  entry->R = R;

  return R;
}

static void cleanup_constrain_cache (void)
{
  register int size = CONSTRAIN_CACHE_SIZE;
  register CONSTRAIN_CACHE_ENTRY *entry = constrain_cache;

  while (size--) {
    if (!BDD_VOID_P (entry->R)) {
      bdd_free (entry->f);
      bdd_free (entry->c);
      bdd_free (entry->R);
      entry->R = BDD_VOID;
    }
    entry++;
  }
  nr_hits = 0;
  nr_lookups = 0;
  nr_collisions = 0;
  occupancy = 0;
}

void print_constrain_cache_info (FILE *fp)
{
fprintf (fp, "*** Constrain Cache Info: ");
fprintf (fp, "%d lookups, %d hits, %d%% success (%d collisions, %d%% occ).\n",
	 nr_lookups, nr_hits,
	 nr_lookups ? nr_hits * 100 / nr_lookups : 100,
	 nr_collisions,
	 (occupancy * 100) / CONSTRAIN_CACHE_SIZE);
}

/* Image Restricting Generalized bdd_cofactor.

   constrain (f, v ) = constrain (fv , 1) = fv
   constrain (f, v') = constrain (fv', 1) = fv'
   constrain (f, ab) = constrain (fa, b) = constrain (fab, 1) = fab

   For a vector function we have:
   The image set of [constrain(f1,c),constrain(f2,c),...,constrain(fn,c)]
   is equal to the image set of [f1, f2,...,fn] with all fi's restricted
   to the domain c.

   Usually it will be the case that f and c are expressed over the same
   set of support variables. If, however, f and c have disjoint (true)
   support then constrain (f, c) will return f.

   Algorithm (ICCAD '90, pp. 131):

   {
     assert (c != 0);
     if (c == 1 || is_constant (f)) return f;
     if (c|r' == 0) return constrain (f|r, c|r);
     if (c|r  == 0) return constrain (f|r', c|r');
     if (f|r  == f|r') [ Optional ]
       return r & constrain (f, c|r) + r' & constrain (f, c|r');
     return r & constrain (f|r, c|r) + r' & constrain (f|r', c|r');
   }

   In fact calculates constrain (f, c) = f | c = f o PIc, i.e.
   f(x)|c = f(PIc(x)),
   where
            PIc (x) = if c(x) = 1 then x
	              else y such that d(x,y) is minimum and c(y) = 1

	    d(x,y)  = Sum (| xi - yi |2^(n-i)), 1 <= i <= n.
                    1<=i<=n

   Example: x = [0,0,1], y = [1,0,0], n = 3
               i=1,2,3

            d(x,y) = |0-1|2^2 + |0-0|2^1 + |1-0|2^0 = 4 + 1 = 5

   Identities:

		       f' | c = (f | c)'

        (f + g + h + ...) | c = (f | c) + (g | c) + (h | c) ...

	(f & g & h & ...) | c = (f | c) & (g | c) & (h | c) ...

	Generally:

        g (f1(x), f2(x), ...) | c = g (f1(x)|c, f2(x)|c, ...)
*/

/* pre:    !BDD_VOID_P (f) && !BDD_VOID_P (c)
        && !BDD_0_P (c) && !BDD_X_P (c)
*/
static BDDPTR bdd_constrain_aux (BDDPTR f, BDDPTR c)
{
  /* f | 1 = f, 1 | c = 1, 0 | c = 0, X | c = X */
  if (BDD_1_P (c) || BDD_TERM_P (f)) /* (BDD_X inclusive) */
    /* c is the universe, so no domain restriction in effect. */
    /* or f is domain independent */
    return bdd_assign (f);

  /* Speed up 1: */
  /* c -> f = 1 ==> f | c = 1 */
  if (BDD_CONTAINS (f, c))
    /* c -> f = 1 (includes f = c), take f(x) = 1 for c(x)=0, then: */
    return bdd_1 ();

  /* Speed up 2: */
  /* c -> f' = 1 ==> f | c = 0 */
  {
    BDDPTR fnot;

    fnot = bdd_not (f);
    if (BDD_CONTAINS (fnot, c)) {
      /* c -> f' = 1, take f(x) = 0 for c(x)=0, then: */
      bdd_free (fnot);
      return bdd_0 ();
    }
    bdd_free (fnot);
  }

  /* Memory function speed up: check cache: */
  {
    BDDPTR R;

    if (!BDD_VOID_P (R = lookup_constrain_cache (f, c)))
      return bdd_assign (R);
  }

  {
    BDDPTR v;
    BDDPTR T = BDD_COFACTOR_POS (c);
    BDDPTR E = BDD_COFACTOR_NEG (c);
    BDDPTR R;
    int rankC = BDD_RANK (c);
    int rankF = BDD_RANK (f);

    /* c = v.T + v'.E */

    if (rankF == rankC) {

      if (BDD_0_P (E) || BDD_X_P (E)) {
	/* c = 0 for var = 0: take f|var and constrain to c|var (= T): */
	R = bdd_constrain_aux (BDD_COFACTOR_POS (f), T);

	return insert_constrain_cache (f, c, R);
      }

      if (BDD_0_P (T) || BDD_X_P (T)) {
	/* c = 0 for var = 1: take f|var' and constrain to c|var' (= E): */
	R = bdd_constrain_aux (BDD_COFACTOR_NEG (f), E);

	return insert_constrain_cache (f, c, R);
      }

      v = bdd_create_var (BDD_VARID (c));
      T = bdd_constrain_aux (BDD_COFACTOR_POS (f), T);
      E = bdd_constrain_aux (BDD_COFACTOR_NEG (f), E);
      R = bdd_ite (v, T, E);
      bdd_free (v);
      bdd_free (T);
      bdd_free (E);

      return insert_constrain_cache (f, c, R);
    }

    if (rankF < rankC) {
      v = bdd_create_var (BDD_VARID (f));
      T = bdd_constrain_aux (BDD_COFACTOR_POS (f), c);
      E = bdd_constrain_aux (BDD_COFACTOR_NEG (f), c);
      R = bdd_ite (v, T, E);
      bdd_free (v);
      bdd_free (T);
      bdd_free (E);

      return insert_constrain_cache (f, c, R);
    }

    /* rankF > rankC */
    {
      if (BDD_0_P (E) || BDD_X_P (E)) {
	/* c = 0 for var = 0: take f and constrain to c|var (= T): */
	R = bdd_constrain_aux (f, T);

	return insert_constrain_cache (f, c, R);
      }

      if (BDD_0_P (T) || BDD_X_P (T)) {
	/* c = 0 for var = 1: take f and constrain to c|var' (= E): */
	R = bdd_constrain_aux (f, E);

	return insert_constrain_cache (f, c, R);
      }

      v = bdd_create_var (BDD_VARID (c));
      T = bdd_constrain_aux (f, T);
      E = bdd_constrain_aux (f, E);
      R = bdd_ite (v, T, E);
      bdd_free (v);
      bdd_free (T);
      bdd_free (E);

      return insert_constrain_cache (f, c, R);
    }
  }
}

BDDPTR bdd_constrain (BDDPTR f, BDDPTR c)
{
  int save_bdd_do_dynamic_ordering;
  BDDPTR R;

  if (BDD_VOID_P (f) || BDD_VOID_P (c) || BDD_0_P (c) || BDD_X_P (c))
    return BDD_VOID;

  save_bdd_do_dynamic_ordering = bdd_do_dynamic_ordering;
  bdd_do_dynamic_ordering = 0;

  R = bdd_constrain_aux (f, c);

  cleanup_constrain_cache ();

  bdd_do_dynamic_ordering = save_bdd_do_dynamic_ordering;

  return R;
}
#endif

/* Guarantees a consistent generalized cofactor result by temporarily
   switching DVO off. Also flushes the cache afterwards.
*/
BDDPTR *bdd_constrain_vec (BDDPTR *F, int i, int j, BDDPTR c)
{
  register BDDPTR *p;
  register int size;
  int save_bdd_do_dynamic_ordering;

  save_bdd_do_dynamic_ordering = bdd_do_dynamic_ordering;
  bdd_do_dynamic_ordering = 0;

  /* Nr. elements to consider: */
  size = j - i + 1;

  /* First element: */
  p = F+i;
  while (size--) {
    BDDPTR R, f = *p;

    R = bdd_constrain (f, c);
    bdd_free (f);
    *p++ = R;
  }
  bdd_do_dynamic_ordering = save_bdd_do_dynamic_ordering;

  return F;
}
