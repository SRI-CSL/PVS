/*
 DOCUMENTATION INFORMATION				         module: BDD
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system	   : Apollo DN3000, HP 9000/S500, IBM RS/6000
 file	   : bdd_fns.c
 unit-title: 
 ref.	   : Efficient Implementation of a BDD Package, Karl S. Brace. DAC'90
 author(s) : Copyright (c) 1990-1998 G.L.J.M. Janssen
 date	   : 27-MAR-1998
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

/* ************************************************************************ */
/* FILE DOCUMENTATION:                                                      */
/*                                                                          */
/* ************************************************************************ */

/* ------------------------------------------------------------------------ */
/* INCLUDES                                                                 */
/* ------------------------------------------------------------------------ */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "alloc.h"
#include "bdd_fns.h"

/* ------------------------------------------------------------------------ */
/* LOCAL DEFINES                                                            */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* LOCAL TYPE DEFINITIONS                                                   */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* LOCAL VARIABLES                                                          */
/* ------------------------------------------------------------------------ */
static char SccsId[] = "%Z%%Y%/%M% %I% %G%";

/* ------------------------------------------------------------------------ */
/* LOCAL FUNCTION PROTOTYPES                                                */
/* ------------------------------------------------------------------------ */


/* ************************************************************************ */
/* FUNCTION DOCUMENTATION:                                                  */
/*                                                                          */
/* ************************************************************************ */

BDDPTR bdd_none_of_list (BDD_LIST args)
{
  /* none(a1,a2,a3,...) = a1'&a2'&a3'&...
     none() = true
  */
  BDDPTR p = bdd_1 ();

  /* When args is list of variables assume they are in increasing
     rank order, so better to do the and-ing from tail to front,
     thus process list in reverse order:
  */
  args = bdd_list_reverse (args);

  BDD_LIST_FOR_EACH_ELEM (args, elem) {
    BDDPTR   arg = BDD_ELEM_CONTENTS (elem);
    BDDPTR n_arg = bdd_not (arg);
    BDDPTR   n_p = bdd_and (p, n_arg);

    bdd_free (p);
    bdd_free (n_arg);
    p = n_p;
  } BDD_LIST_END_FOR_EACH_ELEM;

  /* Undo reversal of args list: */
  args = bdd_list_reverse (args);

  return p;
}

BDDPTR bdd_none_of_vec (BDDPTR *args, int size)
{
  BDDPTR p = bdd_1 ();

  /* Start at last element: */
  args += size;

  while (size--) {
    BDDPTR n_arg = bdd_not (*--args);
    BDDPTR   n_p = bdd_and (p, n_arg);

    bdd_free (p);
    bdd_free (n_arg);
    p = n_p;
  }
  return p;
}

BDDPTR bdd_one_of_list (BDD_LIST args)
{
  /* one_of(a1,a2,a3,...) = a1  & none(a2,a3,...)
                          + a1' & one_of(a2,a3,...)
     one_of() = false
  */
  if (!args)
    return bdd_0 ();

  /* At least 1 argument. */
  {
    BDD_LIST    rest;
    BDDPTR   arg = bdd_assign (BDD_ELEM_CONTENTS (BDD_LIST_FIRST (args)));
    BDDPTR n_arg = bdd_not (arg);
    BDDPTR R, t1, t2;

    if (BDD_LIST_NEXT (BDD_LIST_FIRST (args))) {
      rest = BDD_LIST_CALLOC ();

      /* Fill in new list header; no real copy of list is made. */
      BDD_LIST_FIRST (rest) = BDD_LIST_NEXT (BDD_LIST_FIRST (args));
      BDD_LIST_LAST  (rest) = BDD_LIST_LAST (args);
      BDD_LIST_SET_SIZE (rest, BDD_LIST_SIZE (args) - 1);
    }
    else
      rest = BDD_LIST_NULL;

    R = bdd_none_of_list (rest);
    t1 = bdd_and (arg, R);
    /* t1 = arg1 & none-of (rest) */
    bdd_free (arg);
    bdd_free (R);
    R = bdd_one_of_list (rest);
    t2 = bdd_and (n_arg, R);
    /* t2 = arg1' & one-of (rest) */
    bdd_free (n_arg);
    bdd_free (R);
    R = bdd_or (t1, t2);
    /* R =  arg1 & none-of (rest) V arg1' & one-of (rest) */
    bdd_free (t1);
    bdd_free (t2);

    /* Only free the header, not the list elements: */
    if (rest) BDD_LIST_FREE (rest);

    return R;
  }
}

BDDPTR bdd_one_of_vec (BDDPTR *vec, int size)
{
  if (!vec || !size)
    return bdd_0 ();

  {
    BDDPTR   arg = bdd_assign (*vec++); /* rest = vec */
    BDDPTR n_arg = bdd_not (arg);
    BDDPTR R, t1, t2;

    R = bdd_none_of_vec (vec, --size); /* size = #elems of rest */
    t1 = bdd_and (arg, R);
    /* t1 = arg1 & none-of (rest) */
    bdd_free (arg);
    bdd_free (R);
    R = bdd_one_of_vec (vec, size);
    t2 = bdd_and (n_arg, R);
    /* t2 = arg1' & one-of (rest) */
    bdd_free (n_arg);
    bdd_free (R);
    R = bdd_or (t1, t2);
    /* R =  arg1 & none-of (rest) V arg1' & one-of (rest) */
    bdd_free (t1);
    bdd_free (t2);

    return R;
  }
}

static int sat_aux (BDDPTR f, BYTE *pi, int negate_result)
{
  BDDPTR temp;

  if (BDD_0_P (f) || BDD_X_P (f)) return 0;

  if (BDD_TERM_P (f)) return !negate_result;

  /* Assign FALSE: */
  pi[BDD_VARID (f)] = !!BDD_I_INV_EDGE_P (f);

  temp = BDD_ELSE (f);
  if (BDD_NEG_P (temp)) {
    if (sat_aux (BDD_O_OFF (temp), pi, !negate_result)) return 1;
  }
  else
    if (sat_aux (temp, pi, negate_result)) return 1;

  /* Not successful, try assigning TRUE: */
  pi[BDD_VARID (f)] = !BDD_I_INV_EDGE_P (f);

  temp = BDD_THEN (f);
  if (BDD_NEG_P (temp))
    return sat_aux (BDD_O_OFF (temp), pi, !negate_result);

  return sat_aux (temp, pi, negate_result);
}

/* If f satisfiable returns TRUE and satisfying truth-assignment in pi.

   pi = MALLOC_ARRAY (var_count, BYTE);
   set pi contents to DONTCARE:
   ...
   sat (f, pi);
*/
int bdd_sat (BDDPTR f, BYTE *pi)
{
  int negate_result;

  if (BDD_VOID_P (f))
    return 0;

  /* Pass only positive f's to sat_aux: */
  if (BDD_NEG_P (f)) {
    negate_result = 1;
    f = BDD_O_OFF (f);
  }
  else
    negate_result = 0;

  return sat_aux (f, pi, negate_result);
}

/* Here index(v) is the rank number of the variable v.
   Assign value c(v) to every node v, such that
   c(0-node) = 0, c(1-node) = 1, c(X-node) = 0,
   c(v) =   c(then(v)) * 2^(index(then(v))-index(v) - 1)
          + c(else(v)) * 2^(index(else(v))-index(v) - 1)
   Note: requires index(const) = #vars.
   Size of satisfying set S (= # of satisfying assignments) for the function f
   rooted at node n is now |S(f)| = c(n) * 2^(index(n)-1).
*/

static int global_N;
static BDDPTR VarDomain;

/* Using own Double number package (see double.[ch]). */

/* Returns the position (counted from 0) of v among the domain variables
   specified by the global VarDomain.

   VarDomain must be a positive-literals-only BDD cube.
   BDD_VARID (v) must be present among the domain variables.
   VarDomain may be some constant indicating that the domain
   comprises all variables; in that case position (v) = BDD_RANK (v).

   Example: let BDD_VARID (v) = 7, and let domain = { 1, 3, 5, 7, 9 }, then
   position (v) = 3.
*/
static unsigned long position (BDDPTR v)
{
  unsigned long d;
  BDDPTR p;

  if (BDD_TERM_P (VarDomain))
    /* Count in ranks. */
    return BDD_RANK (v);

  for (d = 0L, p = VarDomain;
       BDD_RANK (p) < BDD_RANK (v);
       p = BDD_COFACTOR_POS (p), d++);

  return d;
}

/* Returns the difference between the positions of the variables
   referred to by the BDD nodes v and w, assuming that
   BDD_RANK (v) <= BDD_RANK (w), with respect to a rank ordered set of
   variables specified by the global VarDomain.

   Returns position (w) - position (v).
*/
static unsigned long distance (BDDPTR v, BDDPTR w)
{
  unsigned long d;
  BDDPTR p;

  if (BDD_TERM_P (VarDomain))
    /* Count in ranks. */
    return BDD_RANK (w) - BDD_RANK (v);

  for (p = VarDomain; BDD_RANK (p) < BDD_RANK (v); p = BDD_COFACTOR_POS (p));
  /* Here: BDD_RANK (p) == BDD_RANK (v) */

  for (d = 0L; BDD_RANK (p) < BDD_RANK (w); p = BDD_COFACTOR_POS (p), d++);

  return d;
}

#if 0
/* This version makes too many rounding mistakes!
   It is only correct when no complemented edges are used.
*/

/* Assumes that there are no BDD_X terminal nodes presents in either
   the f and VarDomain BDDs.
*/
static void bdd_count_sat_aux (BDDPTR f)
{
  if (!BDD_TERM_P (f)) {
    BDDPTR T = BDD_THEN (f);
    BDDPTR E = BDD_ELSE (f);
    Double Count_T, Count_E;

    if (BDD_0_P (T))
      Count_T = Double_0;
    else
    if (BDD_TERM_P (T))
      Count_T = D_2up (global_N - position (f) - 1);
    else
      Count_T = D_times2up (BDD_AUX_D (T), distance (f, T) - 1);
      /* Impossible to have BDD_NEG_P (T) */

    if (BDD_0_P (E))
      Count_E = Double_0;
    else
    if (BDD_TERM_P (E))
      Count_E = D_2up (global_N - position (f) - 1);
    else {
      Count_E = D_times2up (BDD_AUX_D (E), distance (f, E) - 1);

      /* In case of a complemented output edge:
	 #paths to 0 = #total paths - #paths to 1.
	 Need to know the position of E's var.
      */
      if (BDD_NEG_P (E))
	Count_E = D_sub (D_2up (global_N - position (f) - 1), count_E);
    }
    BDD_AUX_D (f) = D_add (Count_T, Count_E);
  }
}

Double bdd_count_sat_assignments (BDDPTR f, BDDPTR domain)
{
  Double count;
  unsigned long d;
  BDDPTR R;

  if (   BDD_VOID_P (f)      || BDD_0_P (f)      || BDD_X_P (f)
      || BDD_VOID_P (domain) || BDD_1_P (domain))
    return Double_0;

  if (BDD_TERM_P (domain))
    global_N = bdd_nr_vars;
  else
    global_N = bdd_size_cube (domain);

  if (BDD_TERM_P (f))
    /* Result is 2^nr_vars: */
    return D_2up (global_N);

  /* Make sure X's are interpreted correctly: */
  R = bdd_on_set (f);

  VarDomain = domain;

  /* Here: !BDD_VOID_P (R) && !BDD_TERM_P (R) */
  /* Watch out! Cannot use regular bdd_traverse_post because that
     might affect VarDomain BDD.
  */
  bdd_traverse_post_rec (R, bdd_count_sat_aux);

  count = BDD_AUX_D (R);

  /* Reinit all the Double aux fields: */
  bdd_traverse_pre (R, bdd_reinit_aux1_and_aux2_action);

  d = position (R);

  if (BDD_NEG_P (R))
    count = D_sub (D_2up (global_N - d), count);

  count = D_times2up (count, d);
  bdd_free (R);
  return count;
}
#endif

#if 0
#define position(v)	BDD_RANK (v)
#define distance(v,w)	(BDD_RANK (w) - BDD_RANK (v))

Double bdd_count_sat_assignments (BDDPTR f)
{
  Double count;
  BDDPTR R;

  if (BDD_VOID_P (f) || BDD_0_P (f) || BDD_X_P (f))
    return Double_0;

  global_N = bdd_nr_vars;

  if (BDD_TERM_P (f))
    /* Result is 2^nr_vars: */
    return D_2up (global_N);

  /* Make sure X's are interpreted correctly: */
  R = bdd_on_set (f);

  /* Here: !BDD_VOID_P (R) && !BDD_TERM_P (R) */
  bdd_traverse_post (R, bdd_count_sat_aux);

  count = BDD_AUX_D (R);

  bdd_traverse_pre (R, bdd_reinit_aux1_and_aux2_action);

  if (BDD_NEG_P (R))
    count = D_sub (D_2up (global_N - BDD_RANK (R)), count);
  count = D_times2up (count, BDD_RANK (R));
  bdd_free (R);
  return count;
}
#endif

/* Expensive version, but correct! */
#define BDD_POS_DPTR(F)	((Double *) BDD_AUX1_PTR (F))
#define BDD_NEG_DPTR(F)	((Double *) BDD_AUX2_PTR (F))

static Double *D_COPY (Double D)
{
  Double *p = MALLOC_STRUCT (Double);

  *p = D;
  return p;
}

/* Assumes that there are no BDD_X terminal nodes presents in either
   the f and VarDomain BDDs.
*/
static void bdd_count_sat_aux (BDDPTR f)
{
  if (!BDD_TERM_P (f)) {
    BDDPTR T = BDD_THEN (f);
    BDDPTR E = BDD_ELSE (f);
    Double Count_T_p, Count_E_p;
    Double Count_T_n, Count_E_n;

    if (BDD_0_P (T)) {
      Count_T_p = Double_0;
      Count_T_n = D_2up (global_N - position (f) - 1);
    }
    else
    if (BDD_TERM_P (T)) {
      Count_T_p = D_2up (global_N - position (f) - 1);
      Count_T_n = Double_0;
    }
    else {
      Count_T_p = D_times2up (*BDD_POS_DPTR (T), distance (f, T) - 1);
      Count_T_n = D_times2up (*BDD_NEG_DPTR (T), distance (f, T) - 1);
      /* Impossible to have BDD_NEG_P (T) */
    }

    if (BDD_0_P (E)) {
      Count_E_p = Double_0;
      Count_E_n = D_2up (global_N - position (f) - 1);
    }
    else
    if (BDD_TERM_P (E)) {
      Count_E_p = D_2up (global_N - position (f) - 1);
      Count_E_n = Double_0;
    }
    else {
      Count_E_p = D_times2up (*BDD_POS_DPTR (E), distance (f, E) - 1);
      Count_E_n = D_times2up (*BDD_NEG_DPTR (E), distance (f, E) - 1);
      if (BDD_NEG_P (E)) {
	Double D = Count_E_p;

	Count_E_p = Count_E_n;
	Count_E_n = D;
      }
    }
    BDD_AUX1_PTR (f) = D_COPY (D_add (Count_T_p, Count_E_p));
    BDD_AUX2_PTR (f) = D_COPY (D_add (Count_T_n, Count_E_n));
  }
}

static void bdd_reset_aux1_and_aux2_action (BDDPTR v)
{
  if (BDD_AUX1_PTR (v)) {
    free (BDD_AUX1_PTR (v));
    BDD_AUX1_PTR (v) = NULL;
  }
  if (BDD_AUX2_PTR (v)) {
    free (BDD_AUX2_PTR (v));
    BDD_AUX2_PTR (v) = NULL;
  }
}

/*
Returns the number of possible truth-assignments to the variables as specified
by the domain argument that make the function f true.
In other words, the number of minterms of the on-set of the function considered
to be defined on the domain.
It is assumed that the support of f is a sub-set of the domain variables.
If not, the program will exit.
The domain argument is supposed to be a BDD cube of positive variables, i.e.,
the conjunction of a number of variables; this cube is treated like a set:
BDD_1 represents the empty set, any other constant represents the universe,
i.e., it implicitly specifies all variables currently present (there are
bdd_nr_vars of them).
New functionality because of domain argument contributed by Arjen Mets.
See also the Double package in double.[ch].
*/
Double bdd_count_sat_assignments (BDDPTR f, BDDPTR domain)
{
  Double count;
  BDDPTR R;

  if (   BDD_VOID_P (f)      || BDD_0_P (f)      || BDD_X_P (f)
      || BDD_VOID_P (domain) || BDD_1_P (domain))
    return Double_0;

  if (BDD_TERM_P (domain))
    global_N = bdd_nr_vars;
  else
    global_N = bdd_size_cube (domain);

  if (BDD_TERM_P (f))
    /* Result is 2^nr_vars: */
    return D_2up (global_N);

  /* Make sure X's are interpreted correctly (must NOT free f!): */
  R = bdd_on_set (f);

  VarDomain = domain;

  /* Here: !BDD_VOID_P (R) && !BDD_TERM_P (R) */
  /* Watch out! Cannot use regular bdd_traverse_post because that
     might affect VarDomain BDD.
  */
  bdd_traverse_post_rec (R, bdd_count_sat_aux);

  count = BDD_NEG_P (R) ? *BDD_NEG_DPTR (R) : *BDD_POS_DPTR (R);

  /* Reinit all the Double aux fields: */
  /* For safety clear the aux fields on constants: */
  bdd_reinit_aux1_and_aux2_action (BDD_0);
  bdd_reinit_aux1_and_aux2_action (BDD_1);
  bdd_traverse_pre (R, bdd_reset_aux1_and_aux2_action);

  count = D_times2up (count, position (R));
  bdd_free (R);
  return count;
}

/* Pre: !BDD_VOID_P (f) */
/* Note: BDD_RANK values assumed to start at 0. */
static void bdd_count_X_terms_aux (BDDPTR f)
{
  if (BDD_INTERN_P (f)) {
    BDDPTR T = BDD_THEN (f);
    BDDPTR E = BDD_ELSE (f);
    Double Count_T, Count_E;

    if (BDD_X_P (T))
      Count_T = D_2up (global_N - 1 - BDD_RANK (f));
    else
    if (BDD_TERM_P (T))
      Count_T = Double_0;
    else {
      Count_T = D_times2up (BDD_AUX_D (T), BDD_RANK (T) - BDD_RANK (f) - 1);
      /* Impossible to have BDD_NEG_P (T) */
    }

    if (BDD_X_P (E))
      Count_E = D_2up (global_N - 1 - BDD_RANK (f));
    else
    if (BDD_TERM_P (E))
      Count_E = Double_0;
    else {
      Count_E = D_times2up (BDD_AUX_D (E), BDD_RANK (E) - BDD_RANK (f) - 1);
    }
    BDD_AUX_D (f) = D_add (Count_T, Count_E);
  }
}

/* Count #assignments to variables that make f BDD_X. */
Double bdd_count_X_terms (BDDPTR f)
{
  Double count;

  if (BDD_VOID_P (f) || BDD_TERM_P (f) && !BDD_X_P (f))
    return Double_0;

  global_N = bdd_nr_vars;

  if (BDD_X_P (f))
    /* Result is 2^nr_vars: */
    return D_2up (global_N);

  /* Here: !BDD_VOID_P (f) && !BDD_TERM_P (f) */
  bdd_traverse_post (f, bdd_count_X_terms_aux);

  count = BDD_AUX_D (f);

  bdd_traverse_pre (f, bdd_reinit_aux1_and_aux2_action);

  return D_times2up (count, BDD_RANK (f));
}

/* WATCH OUT: rank values of BDD variables may change when new
   variables are created. This only happens when the rank table is
   used (in combination with bdd_create_var_first, _last, or _after).

   Therefore in routines that create new variables (i.e. ones that did
   not exist before) one should not rely on a rank value kept in
   a temporary C variable. Better is always to calculate rank values
   afresh via the BDD_RANK or BDD_VAR_RANK macros.
*/

/* pre: !BDD_VOID_P (f) && rank < BDD_TERMID */
static int bdd_in_support_aux (int rank, BDDPTR f)
{
  int topF;

 restart:

  /* Mark where we are going: */
  BDD_TOGGLE_MARK (f);

  topF = BDD_RANK (f);

  /* Bottom case 1: */
  if (rank < topF)
    /* Covers the case that BDD_TERM_P (f). */
    /* Definitely no need to look further: */
    return 0;

  /* Bottom case 2: */
  if (rank == topF)
    /* Found the variable; can stop search positively. */
    return 1;

  /* Recursive case: */
  if (BDD_NOT_MARKED (BDD_THEN (f), f))
    /* Not yet been there, so recurse: */
    if (bdd_in_support_aux (rank, BDD_THEN (f)))
      /* Aha, found variable so can stop search positively. */
      return 1;
    /* else: Not found; still need to investigate ELSE side. */
  /* else: Already looked there; still need to investigate ELSE side. */

  if (BDD_MARKED (BDD_ELSE (f), f))
    /* Already looked there; no need to look further. */
    return 0;

  /* Avoid tail-recursion: */
  f = BDD_ELSE (f);
  goto restart;
}

int bdd_in_support (int var, BDDPTR f)
{
  int rank = BDD_VAR_RANK (var);
  int result;

  if (BDD_VOID_P (f))
    return 0;

  if (rank == BDD_TERMID || rank < BDD_RANK (f))
    /* Covers the case that BDD_TERM_P (f). */
    return 0;

  result = bdd_in_support_aux (rank, f);
  bdd_reset_marks (f);
  return result;
}

/* pre: !BDD_VOID_P (f).
   supF is the address of a list of elements, each contains a variable
   id as contents. The list is in increasing order of the variables'
   rank numbers. Upon completion, the list reflects the true support
   of f.
*/
static void support_as_list_of_vars_aux (BDDPTR f, BDD_ELEM *supF)
{
 restart:

  /* Mark wherever we go: */
  BDD_TOGGLE_MARK (f);

  if (BDD_INTERN_P (f)) {
    BDD_ELEM sup_f, prev;
    BDDPTR T = BDD_THEN (f);
    BDDPTR E = BDD_ELSE (f);
    int rankF = BDD_RANK (f);
    int equalrank = 0;

    /* Skip as many elements in supF till one that has rank >= BDD_RANK (f),
       i.e. get the partial support of f.
    */
    sup_f = *supF;
    prev  = NULL;
    while (sup_f) {
      int varid = BDD_ELEM_CONTENTS_I (sup_f);
      int rank  = BDD_VAR_RANK (varid);

      if (rank >= rankF) {
	equalrank = rank == rankF;
	break;
      }
      prev  = sup_f;
      sup_f = BDD_LIST_NEXT (sup_f);
    }
    /* First element in sup_f has rank >= BDD_RANK (f) or sup_f empty list. */

    if (equalrank) {		/* ==> sup_f != NULL */
      /* Element already present: support of f starts with sup_f. */
    }
    else {			/* !sup_f or rank > rankC */
      /* Insert/append a new element after the prev one: */
      BDD_ELEM new_elem;

      new_elem = BDD_ELEM_CALLOC ();
      BDD_ELEM_SET_CONTENTS (new_elem, BDD_VARID (f));
      BDD_LIST_NEXT (new_elem) = sup_f;
      if (prev)
	BDD_LIST_NEXT (prev) = new_elem;
      else
	*supF = new_elem;
      sup_f = new_elem;
    }

    if (BDD_NOT_MARKED (T, f))
      /* Not yet been there, so recurse: */
      support_as_list_of_vars_aux (T, &(BDD_LIST_NEXT (sup_f)));

    if (BDD_NOT_MARKED (E, f)) {
      /* Avoid tail-recursion:
	 support_as_list_of_vars_aux (E, &(BDD_LIST_NEXT (sup_f)));
      */
      f = E;
      supF = &(BDD_LIST_NEXT (sup_f));
      goto restart;
    }
  }
}

BDD_LIST bdd_support_as_list_of_vars (BDDPTR f)
{
  if (BDD_VOID_P (f) || BDD_TERM_P (f))
    return BDD_LIST_NULL;

  {
    /* No variables seen so far: */
    BDD_ELEM end_p;
    BDD_ELEM next;
    BDD_LIST vars = BDD_LIST_CALLOC ();
    int size = 1;

    /* Always finds at least 1 variable. */
    /* Initially: BDD_LIST_FIRST (vars) == NULL */
    support_as_list_of_vars_aux (f, &(BDD_LIST_FIRST (vars)));
    bdd_reset_marks (f);
    /* start_p != NULL; at least 1 variable in BDD f. */
    end_p = BDD_LIST_FIRST (vars);
    next  = BDD_LIST_NEXT (BDD_LIST_FIRST (vars));

    /* Determine last element of list and also nr. of elements: */
    while (next) {
      end_p = next;
      next  = BDD_LIST_NEXT (next);
      size++;
    }
    /* Establish the list header record: */
    BDD_LIST_LAST (vars) = end_p;
    BDD_LIST_SET_SIZE (vars, size);
    return vars;
  }
}

int bdd_nr_support_vars (BDDPTR f)
{
  BDD_LIST vars = bdd_support_as_list_of_vars (f);
  int size  = BDD_LIST_SIZE (vars);

  bdd_list_free (vars, 0);
  return size;
}

/* Returns cube representing the AND over all var ids in the `vars'.
   Negative var ids will be interpreted to mean negative literals.
   Assumes var ids are in rank order, but this is not mandatory.
   `vars' may contain multiple occurrences of the same var id.
   If `vars' is empty BDD_1 is returned.
*/
BDDPTR bdd_list_of_vars_as_cube (BDD_LIST vars)
{
  BDDPTR cube = bdd_1 ();

  /* Like to have var id's in decreasing rank order,
     AND-ing is faster then.
  */
  vars = bdd_list_reverse (vars);

  /* From highest to lowest variable rank: */
  BDD_LIST_FOR_EACH_ELEM (vars, elem) {
    BDDPTR R, var;

    var  = bdd_create_var (BDD_ELEM_CONTENTS_I (elem));
    cube = bdd_and (var, R = cube);
    bdd_free (var);
    bdd_free (R);
  } BDD_LIST_END_FOR_EACH_ELEM;

  return cube;
}

BDDPTR bdd_support_as_cube (BDDPTR f)
{
  if (BDD_VOID_P (f))
    return BDD_VOID;

  if (BDD_TERM_P (f))
    return bdd_1 ();

  {
    BDD_LIST vars = bdd_support_as_list_of_vars (f);
    BDDPTR   cube = bdd_list_of_vars_as_cube (vars);

    bdd_list_free (vars, 0);
    return cube;
  }
}

static int rank_less_or_equal (void *a, void *b)
{
  return BDD_VAR_RANK ((int) a) - BDD_VAR_RANK ((int) b);
}

/* Destructively reorders `vars' to obtain all variable elements in
   increasing rank order.
*/
BDD_LIST bdd_rank_order_vars (BDD_LIST vars)
{
  return bdd_list_mergesort (vars, rank_less_or_equal);
}

static BDD_LIST list_of_cube_varids;

static void collect_cube_vars_action (int index, int neg, int first)
{
  list_of_cube_varids = bdd_list_append_cont ((void *) (neg ? -index : index),
					      list_of_cube_varids);
}

BDD_LIST bdd_cube_as_list_of_vars (BDDPTR cube)
{
  list_of_cube_varids = BDD_LIST_NULL;
  bdd_traverse_cube (cube, collect_cube_vars_action);
  return list_of_cube_varids;
}

/* Modify the value stored in the aux1 field of f according to the
   modification bits of f:
   BDD_O_INV_EDGE_P (f) means complement BDD_AUX1_BDD (f), and/or
   BDD_I_INV_EDGE_P (f) means invert the top-variable of BDD_AUX1_BDD (f).
   Result is protected.
   pre: !BDD_VOID_P (f).
*/
BDDPTR bdd_invert_input_interpret_mod_bits (BDDPTR f)
{
  BDDPTR R = BDD_AUX1_BDD (f);	/* !BDD_VOID_P (R) */

  /* R = (PTR (f))|v=v'. */
  if (BDD_I_INV_EDGE_P (f))
    /* R non-constant. */
    R = bdd_invert_input_top (R);
  else
    bdd_assign (R);

  if (BDD_O_INV_EDGE_P (f))
    /* R could be a constant. */
    /* Simply complement the negated-output bit: */
    R = BDD_COMPL (R);

  return R;
}

/* pre: !BDD_VOID_P (f) && rank < BDD_TERMID. */
static void bdd_invert_input_aux (BDDPTR f, int rank)
{
  /* Mark wherever we go: */
  BDD_TOGGLE_MARK (f);

  /* f could be a constant! */

  /* Bottom case 1: */
  if (rank < BDD_RANK (f)) {
    /* Covers the case that BDD_TERM_P (f). */
    /* Invert-input bit on constant is not possible! */
    /* var not in the support of f. */
    bdd_assign (f);
    BDD_AUX1_BDD (f) = PTR (f);
    /* Clearly, f not a constant then top-var of result equals top-var of f. */
    return;
  }

  /* BDD_RANK (f) <= rank < BDD_TERMID. */
  {
    BDDPTR v = bdd_create_var (BDD_VARID (f));
    BDDPTR T = BDD_THEN (f);
    BDDPTR E = BDD_ELSE (f);

    /* Bottom case 2: */
    if (rank == BDD_RANK (f)) {
      /* Let F = PTR (f) = v.T + v'.E
	 Here x = v, so F|x=x' = x'.T + x.E = x.E + x'.T,
	 since x not in support of T and E.
	 Clearly, top-var of result is also v.
      */
      BDD_AUX1_BDD (f) = bdd_ite (v, E, T);
      bdd_free (v);
      return;
    }

    /* Recursive case: */

    /* Let F = PTR (f) = v.T + v'.E
       F|x=x' = v.(T|x=x') + v'.(E|x=x').
       We know: T != E ==> T|x=x' != E|x=x',
                v not in support of either T or E
		==> top-var of result must be v
    */
    if (BDD_NOT_MARKED (T, f))
      /* Not yet been there, so recurse: */
      bdd_invert_input_aux (T, rank);

    if (BDD_NOT_MARKED (E, f))
      /* Not yet been there, so recurse: */
      bdd_invert_input_aux (E, rank);

    T = bdd_invert_input_interpret_mod_bits (T);
    E = bdd_invert_input_interpret_mod_bits (E);

    BDD_AUX1_BDD (f) = bdd_ite (v, T, E);
    bdd_free (v);
    bdd_free (T);
    bdd_free (E);
  }
}

/* Note: uses aux1 field to hold result of (PTR (f)|v=v'). */
BDDPTR bdd_invert_input (BDDPTR f, int var)
{
  int save_bdd_do_dynamic_ordering;
  int rank = BDD_VAR_RANK (var);
  BDDPTR R;

  if (BDD_VOID_P (f))
    return BDD_VOID;

  if (rank == BDD_TERMID || rank < BDD_RANK (f))
    /* Covers the case that BDD_TERM_P (f). */
    /* var not in the support of f. */
    return bdd_assign (f);

  /* f non-constant. */

  /* Speed-up for frequent case: */
  if (rank == BDD_RANK (f))
    return bdd_invert_input_top (f);

  save_bdd_do_dynamic_ordering = bdd_do_dynamic_ordering;
  bdd_do_dynamic_ordering = 0;

  bdd_invert_input_aux (f, rank);
  R = bdd_invert_input_interpret_mod_bits (f);

  bdd_do_dynamic_ordering = save_bdd_do_dynamic_ordering;

  /* Also resets marks: */
  bdd_traverse_pre (f, bdd_free_aux1_action);

  return R;
}

/* pre: !BDD_VOID_P (f) */
static void bdd_invert_inputs_aux (BDDPTR f)
{
  /* Mark wherever we go: */
  BDD_TOGGLE_MARK (f);

  /* f could be a constant! */

  /* Bottom case: */
  if (BDD_TERM_P (f)) {
    /* Invert-input bit on constant is not possible! */
    bdd_assign (f);
    BDD_AUX1_BDD (f) = PTR (f);
    return;
  }

  /* Recursive case: */
  {
    BDDPTR v;
    BDDPTR T = BDD_THEN (f);
    BDDPTR E = BDD_ELSE (f);

    /* Let F = PTR (f) = v.T + v'.E
       inv (F) = v'.inv(T) + v.inv(E).
       We know: T != E ==> inv(T) != inv(E),
                v not in support of either T or E
		==> top-var of result must be v
    */
    if (BDD_NOT_MARKED (T, f))
      /* Not yet been there, so recurse: */
      bdd_invert_inputs_aux (T);

    if (BDD_NOT_MARKED (E, f))
      /* Not yet been there, so recurse: */
      bdd_invert_inputs_aux (E);

    v = bdd_create_var (BDD_VARID (f));
    T = bdd_invert_input_interpret_mod_bits (T);
    E = bdd_invert_input_interpret_mod_bits (E);

    BDD_AUX1_BDD (f) = bdd_ite (v, E, T);
    bdd_free (v);
    bdd_free (T);
    bdd_free (E);
  }
}

BDDPTR bdd_invert_inputs (BDDPTR f)
{
  int save_bdd_do_dynamic_ordering;
  BDDPTR R;

  if (BDD_VOID_P (f))
    return BDD_VOID;

  if (BDD_TERM_P (f))
    return bdd_assign (f);

  save_bdd_do_dynamic_ordering = bdd_do_dynamic_ordering;
  bdd_do_dynamic_ordering = 0;

  bdd_invert_inputs_aux (f);
  R = bdd_invert_input_interpret_mod_bits (f);

  bdd_do_dynamic_ordering = save_bdd_do_dynamic_ordering;

  /* Also resets marks: */
  bdd_traverse_pre (f, bdd_free_aux1_action);

  return R;
}

/* Interpret the BDD's for substitution results stored in the aux fields of f
   according the modification bits of f.

   BDD_AUX1_BDD (f) = subst (f, var,  PTR (g))
   BDD_AUX2_BDD (f) = subst (f, var, ~PTR (g))

   Where ~ means the inversion of the top-variable.
   Note: aux2 field only necessary when bdd_use_inv_edges = 1.
   Result is protected.
   pre: !BDD_VOID_P (g).
*/
static BDDPTR subst_interpret_mod_bits (BDDPTR g)
{
  BDDPTR R = BDD_I_INV_EDGE_P (g) ? BDD_AUX2_BDD (g) : BDD_AUX1_BDD (g);

  if (BDD_O_INV_EDGE_P (g))
    R = BDD_COMPL (R);

  return bdd_assign (R);
}

/* pre: !BDD_VOID_P (g) && !BDD_VOID_P (f) && rank < BDD_TERMID.
   Uses aux1 field to store non-inverted input result;
   Uses aux2 field to store     inverted input result.
*/
static void bdd_subst_aux (BDDPTR f, int rank, BDDPTR g)
{
  int topG = BDD_RANK (g);

  /* Mark wherever we go: */
  BDD_TOGGLE_MARK (g);

  /* g could be a constant! */

  /* Bottom case 1: */
  if (rank < topG) {
    /* Covers the case that BDD_TERM_P (g). */
    bdd_assign (g);
    BDD_AUX1_BDD (g) = PTR (g);

    if (bdd_use_inv_edges) {
      if (BDD_TERM_P (g)) {
	/* Invert-input on constant is not possible! */
	BDD_AUX2_BDD (g) = BDD_VOID;
      }
      else {
	/* var not in the support of g. */
	BDDPTR v = bdd_create_var (BDD_VARID (g));

	BDD_AUX2_BDD (g) = bdd_ite (v, BDD_ELSE (g), BDD_THEN (g));
	bdd_free (v);
      }
    }
    return;
  }

  /* Here: topG <= rank < BDD_TERMID */
  {
    BDDPTR v;
    BDDPTR T = BDD_THEN (g);
    BDDPTR E = BDD_ELSE (g);

    /* Bottom case 2: */
    if (rank == topG) {
      /* Since rank != BDD_TERMID ==> g not a constant. */
      /* 4 cases for g:

	 1)  v .T + v'.E   --> R = f.T + f'.E
	 2)  v'.T + v .E   --> R = f.E + f'.T Special!
	 3) (v .T + v'.E)' --> R = (f.T + f'.E)' = f.T' + f'.E'
	 4) (v'.T + v .E)' --> R = (f.E + f'.T)' = f.E' + f'.T' Special!
      */
      BDD_AUX1_BDD (g) = bdd_ite (f, T, E);
      if (bdd_use_inv_edges)
      BDD_AUX2_BDD (g) = bdd_ite (f, E, T);
      return;
    }

    /* Here: rank > topG. */

    /* Recursive case: */
    if (BDD_NOT_MARKED (T, g))
      /* Not yet been there, so recurse: */
      bdd_subst_aux (f, rank, T);

    if (BDD_NOT_MARKED (E, g))
      /* Not yet been there, so recurse: */
      bdd_subst_aux (f, rank, E);

    v = bdd_create_var (BDD_VARID (g));
    T = subst_interpret_mod_bits (T);
    E = subst_interpret_mod_bits (E);

    BDD_AUX1_BDD (g) = bdd_ite (v, T, E);
    if (bdd_use_inv_edges)
    BDD_AUX2_BDD (g) = bdd_ite (v, E, T);
    bdd_free (v);
    bdd_free (T);
    bdd_free (E);
  }
}

/* Substitute f for (positive occurrences of) var in g.
   Note: g'|(var=f) = (g|(var=f))'.

   Notation: g[f/var]
   Note: g[var1/var] = E var.(var == var1 and g)
*/
BDDPTR bdd_subst (BDDPTR f, int var, BDDPTR g)
{
  int save_bdd_do_dynamic_ordering;
  int rank = BDD_VAR_RANK (var);
  BDDPTR R;

  if (BDD_VOID_P (f) || BDD_VOID_P (g))
    return BDD_VOID;

  if (rank == BDD_TERMID || rank < BDD_RANK (g))
    /* Covers the case that BDD_TERM_P (g). */
    /* rank not in the support of g. */
    return bdd_assign (g);

  /* g non-constant. */

  /* Speed-up for frequent case: */
  if (rank == BDD_RANK (g)) {
    BDDPTR v, T, E;

    bdd_cofactors (g, &v, &T, &E);
    R = bdd_ite (f, T, E);
    bdd_free (v);
    bdd_free (T);
    bdd_free (E);
    return R;
  }

  save_bdd_do_dynamic_ordering = bdd_do_dynamic_ordering;
  bdd_do_dynamic_ordering = 0;

  bdd_subst_aux (f, rank, g);
  R = subst_interpret_mod_bits (g);

  bdd_do_dynamic_ordering = save_bdd_do_dynamic_ordering;

  /* Also resets marks: */
  if (bdd_use_inv_edges)
    bdd_traverse_pre (g, bdd_free_aux1_and_aux2_action);
  else
    bdd_traverse_pre (g, bdd_free_aux1_action);

  return R;
}

BDDPTR *bdd_subst_vec (BDDPTR *F, int i, int j, int var, BDDPTR g)
{
  int save_bdd_do_dynamic_ordering;
  register BDDPTR *p;
  register int k;
  int rank = BDD_VAR_RANK (var);

  save_bdd_do_dynamic_ordering = bdd_do_dynamic_ordering;
  bdd_do_dynamic_ordering = 0;

  /* All F[k] assumed to be NOT marked! */
  for (k = i, p = F+i; k <= j; k++, p++) {
    BDDPTR f = *p;

    if (!BDD_VOID_P (f)) {

      if (BDD_MARK (f)) {
	BDDPTR R = subst_interpret_mod_bits (f);

	bdd_free (f);
	*p = R;
      }
      else
      if (rank == BDD_TERMID || rank < BDD_RANK (f)) {
	/* Covers the case that BDD_TERM_P (f). */
	/* Result is f. */
      }
      else
	bdd_subst_aux (g, rank, f);
    }
  } /*for k*/

  for (k = i, p = F+i; k <= j; k++, p++) {
    BDDPTR f = *p;

    if (!BDD_VOID_P (f) && BDD_MARK (f)) {
      BDDPTR R = subst_interpret_mod_bits (f);

      /* Also resets marks: */
      if (bdd_use_inv_edges)
	bdd_traverse_pre (f, bdd_free_aux1_and_aux2_action);
      else
	bdd_traverse_pre (f, bdd_free_aux1_action);

      bdd_free (f);
      *p = R;
    }
  }
  bdd_do_dynamic_ordering = save_bdd_do_dynamic_ordering;

  return F;
}

typedef struct {
  int var_rank;
  BDDPTR sub;
} subst_rec;

/* pre: !BDD_VOID_P (g)
   Uses aux1 field to store non-inverted input result;
   Uses aux2 field to store     inverted input result.
*/
static void bdd_subst_par_aux (subst_rec *substs, BDDPTR g)
{
  int topG = BDD_RANK (g);

  /* Mark wherever we go: */
  BDD_TOGGLE_MARK (g);

  /* Skip vars that are definitely not in support of g: */
  while (substs->var_rank < topG)
    substs++;

  if (substs->var_rank == BDD_TERMID) {
    /* Hit sentinel. */
    bdd_assign (g);
    BDD_AUX1_BDD (g) = PTR (g);
    if (bdd_use_inv_edges)
      BDD_AUX2_BDD (g) = bdd_invert_input_top (PTR (g));
    return;
  }

  /* substs->var_rank >= topG. */
  /* g cannot be a constant! */

  {
    BDDPTR v;
    BDDPTR T = BDD_THEN (g);
    BDDPTR E = BDD_ELSE (g);

    if (BDD_NOT_MARKED (T, g))
      bdd_subst_par_aux (substs, T);

    if (BDD_NOT_MARKED (E, g))
      bdd_subst_par_aux (substs, E);
    
    T = subst_interpret_mod_bits (T);
    E = subst_interpret_mod_bits (E);

    if (substs->var_rank == topG)
      v = bdd_assign (substs->sub);
    else
      v = bdd_create_var (BDD_VARID (g));

    BDD_AUX1_BDD (g) = bdd_ite (v, T, E);
    if (bdd_use_inv_edges)
    BDD_AUX2_BDD (g) = bdd_ite (v, E, T);
    bdd_free (v);
    bdd_free (T);
    bdd_free (E);
  }
}

static int subst_par_comp (const subst_rec *p, const subst_rec *q)
{
  return p->var_rank - q->var_rank;
}

/* Substitute f_vec[i] for (positive occurrences of) vars[i] in g.
   Substitution of all vars[i]'s is done in parallel.

   vars is list of var id's assumed to be all distinct.
   This routine takes care of treating the vars in proper rank order.
   Note that during bdd_subst_par dynamic variable ordering is OFF.
   It is assumed that there is a f_vec[i] for each element of the vars list.
   The correspondence is:
     f_vec[0] ~ first element in vars
     f_vec[1] ~ 2-nd  element in vars
     ...
     f_vec[BDD_LIST_SIZE (vars)-1] ~ last  element in vars
   Any f_vec[i] that are BDD_VOID cause the substitution for the corresponding
   variable to be skipped.
   Returns (copy of) g immediately when g is BDD_VOID or g is a constant or
   when no substitution needs to take place, i.e., vars is an empty list or
   all f_vec[i] are BDD_VOID.
*/
BDDPTR bdd_subst_par (BDDPTR *f_vec, BDD_LIST vars, BDDPTR g)
{
  int save_bdd_do_dynamic_ordering;
  BDDPTR R;
  subst_rec *substs;
  int nr_substs, nr_vars;

  if (!vars || BDD_VOID_P (g) || BDD_TERM_P (g))
    return bdd_assign (g);

  nr_vars = BDD_LIST_SIZE (vars);
  substs  = MALLOC_ARRAY (nr_vars+1, subst_rec);
  nr_substs = 0;
  BDD_LIST_FOR_EACH_ELEM (vars, elem) {
    if (!BDD_VOID_P (*f_vec)) {
      substs[nr_substs  ].sub      = *f_vec;
      substs[nr_substs++].var_rank = BDD_VAR_RANK (BDD_ELEM_CONTENTS_I (elem));
    }
    f_vec++;
  } BDD_LIST_END_FOR_EACH_ELEM;

  if (nr_substs) {
    qsort ((void *) substs, (size_t) nr_substs,
	   (size_t) sizeof (subst_rec),
	   (int (*) (const void *, const void *)) subst_par_comp);

    /* Attach sentinel element: */
    substs[nr_substs].sub      = BDD_VOID;
    substs[nr_substs].var_rank = BDD_TERMID;

    save_bdd_do_dynamic_ordering = bdd_do_dynamic_ordering;
    bdd_do_dynamic_ordering = 0;

    bdd_subst_par_aux (substs, g);
    R = subst_interpret_mod_bits (g);

    bdd_do_dynamic_ordering = save_bdd_do_dynamic_ordering;

    /* Also resets marks: */
    if (bdd_use_inv_edges)
      bdd_traverse_pre (g, bdd_free_aux1_and_aux2_action);
    else
      bdd_traverse_pre (g, bdd_free_aux1_action);
  }
  else
    R = bdd_assign (g);

  MA_FREE_ARRAY (substs, nr_vars+1, subst_rec);
  return R;
}

/* Replace var BDD_VARID (g) in f by h. */
BDDPTR bdd_compose (BDDPTR f, BDDPTR g, BDDPTR h)
{
  if (BDD_VOID_P (f) || BDD_VOID_P (g) || BDD_VOID_P (h))
    return BDD_VOID;

  if (BDD_0_P (g))
    return bdd_or (f, h);

  if (BDD_1_P (g))
    return bdd_and (f, h);

  if (BDD_X_P (g))
    return bdd_replace_X (f, h);

  if (BDD_TERM_P (g))
    return BDD_VOID;

  /* Substitute h for g in f. */
  return bdd_subst (h, BDD_VARID (g), f);
}

int bdd_cube_p (BDDPTR f)
{
  if (BDD_0_P (f) || BDD_1_P (f))
    return 1;

  if (BDD_VOID_P (f) || BDD_TERM_P (f))
    return 0;

  /* Here f non-constant. */
  do {
    BDDPTR T = BDD_COFACTOR_POS (f);
    BDDPTR E = BDD_COFACTOR_NEG (f);

    /* In a valid bdd cube each node has at least 1 constant child,
       and if only 1 then it must be the BDD_0.
       The bottom node has 2 constant children and they must be
       BDD_0 and BDD_1.
       There may not be any X's in a cube.
    */

    if (BDD_X_P (T) || BDD_X_P (E)) return 0;

    if (BDD_0_P (T))
      /* E when constant is guaranteed to be 1, otherwise must proceed. */
      f = E;
    else
    if (BDD_0_P (E))
      /* T when constant is guaranteed to be 1, otherwise must proceed. */
      f = T;
    else
      /* Both successors are non-0. */
      return 0;
  } while (BDD_INTERN_P (f));

  /* Here T and E are both constant nodes: either T = 0 and E = 1,
     or T = 1 and E = 0.
  */
  return 1;
}

static int bdd_has_dontcare_aux (BDDPTR f)
{
  /* (Depth-First) Search for a DONTCARE constant node: */
 restart:

  BDD_TOGGLE_MARK (f);

  if (BDD_X_P (f))
    return 1;

  if (BDD_TERM_P (f))
    return 0;

  /* Here f non-constant. */
  if (BDD_NOT_MARKED (BDD_THEN (f), f))
    if (bdd_has_dontcare_aux (BDD_THEN (f)))
      return 1;

  if (BDD_MARKED (BDD_ELSE (f), f))
    return 0;

  f = BDD_ELSE (f);
  goto restart;
}

int bdd_has_dontcare (BDDPTR f)
{
  int result;

  if (BDD_VOID_P (f))
    return 0;

  result = bdd_has_dontcare_aux (f);
  bdd_reset_marks (f);
  return result;
}

/* Suppose a function f has indeed don't cares. We can view such a function
   as the result of the operation bdd_ite (h, X, g), i.e. if h then X else g,
   with g and h being functions without don't cares.
   g is usually called the on-set and the function h is called the don't
   care-set, it specifies what input patterns for g we don't care about.
   In order to reconstruct a g from f, we have to create a BDD identical to
   f except that any BDD_X nodes are replaced by BDD_0 nodes.
   This is what on_set (f) does. By the way, if f is a proper function then
   on_set (f) = f. In the replacement of 0 for X care is taken of complemented
   outputs and inverted inputs. In a picture of the BDD this substitution
   is therefore not always obvious.
*/

/*
   Note: For all minterms x elem B^n:

   Definition of on_set:
   f(x) = 0 --> (on_set (f)) (x) = 0,
   f(x) = 1 --> (on_set (f)) (x) = 1,
   f(x) = X --> (on_set (f)) (x) = 0,

   on_set (f) = (Xset'(f) & f), proof:

                Xset(f) (x) | Xset'(f) (x) | (Xset'(f) & f) (x)
   f(x) = 0 -->       0     |        1     |      0
   f(x) = 1 -->       0     |        1     |      1
   f(x) = X -->       1     |        0     |      0

   on_set (f') = (Xset' (f') & f') = Xset' (f) & f'
*/
BDDPTR bdd_on_set (BDDPTR f)
{
  BDDPTR dc, R;

  dc = bdd_dontcare_set (f);
  /* R = dc' and f: */
  R = bdd_ite (dc, BDD_0, f);
  bdd_free (dc);
  return R;
}

/*
   Note: For all minterms x elem B^n:

   Definition of off_set:
   f(x) = 0 --> (off_set (f)) (x) = 1,
   f(x) = 1 --> (off_set (f)) (x) = 0,
   f(x) = X --> (off_set (f)) (x) = 0,

   off_set (f) = (Xset'(f) & f'), proof:

                Xset(f) (x) | Xset'(f) (x) | (Xset'(f) & f') (x)
   f(x) = 0 -->       0     |        1     |      1
   f(x) = 1 -->       0     |        1     |      0
   f(x) = X -->       1     |        0     |      0

   off_set (f') = (Xset' (f') & f'') = Xset' (f) & f = on_set (f)
*/
BDDPTR bdd_off_set (BDDPTR f)
{
  BDDPTR R1, R2;

  R1 = bdd_not (f);
  R2 = bdd_on_set (R1);
  bdd_free (R1);

  return R2;
}

/*
   Note: For all minterms x elem B^n:

   Definition of replace_X:
   f(x) = 0 --> (replace_X (f, h)) (x) = 0,
   f(x) = 1 --> (replace_X (f, h)) (x) = 1,
   f(x) = X --> (replace_X (f, h)) (x) = h(x),

   replace_X (f,h) = Xset (f)' & f + Xset (f) & h, proof:

                                            f(x) =
					    0 | 1 | X
   Xset(f) (x)                            = 0 | 0 | 1
   Xset'(f) (x)         		  = 1 | 1 | 0
   (Xset'(f) & f) (x) 			  = 0 | 1 | 0
   (Xset(f) & h) (x)			  = 0 | 0 | h(x)
   (Xset'(f) & f) (x) + (Xset(f) & h) (x) = 0 | 1 | h(x)

   replace_X (f,h) = ite (Xset'(f), f, h)
                   = ite (Xset(f), h, f)
*/
BDDPTR bdd_replace_X (BDDPTR f, BDDPTR X_is)
{
  BDDPTR Xset_f, R;

  Xset_f = bdd_dontcare_set (f);
  R = bdd_ite (Xset_f, X_is, f);
  bdd_free (Xset_f);

  return R;
}

static BDDPTR dontcare_set_interpret_mod_bits (BDDPTR f)
{
  BDDPTR R = BDD_AUX1_BDD (f);	/* !BDD_VOID_P (R) */

  if (BDD_I_INV_EDGE_P (f) && BDD_VARID (f) == BDD_VARID (R))
    /* R non-constant. Top-var of f might have disappeared in result R. */
    R = bdd_invert_input_top (R);
  else
    bdd_assign (R);

  return R;
}

/* pre: !BDD_VOID_P (f).

   Note: For all minterms x elem B^n:

   Definition of Xset:
   f(x) = 0 --> (Xset (f)) (x) = 0,
   f(x) = 1 --> (Xset (f)) (x) = 0,
   f(x) = X --> (Xset (f)) (x) = 1,

   It follows (using f'(x) = (f(x))'):
   f'(x) = 1 --> (Xset (f')) (x) = 0,
   f'(x) = 0 --> (Xset (f')) (x) = 0,
   f'(x) = X --> (Xset (f')) (x) = 1,

   i.e. result is invariant under complement of argument (symmetric):
   Xset (f') = Xset (f)

   For f = v.T + v'.E we have:
   Xset (f) = v.Xset (T) + v'.Xset (E), where Xset (T) might equal Xset (E),
   and then v does not appear in the result Xset (f).

   Possibilities for T and E values:

   1. Both T and E have no X's present, then Xset (T) = Xset (E) = 0.
   2. T and E both have X's present, and then perhaps Xset (T) = Xset (E),
      because T and E not necessarily agree in 0, 1 function results.
   3. T has some X's and E not, or the other way around, then for sure
      Xset (T) != Xset (E).
*/
static void bdd_dontcare_set_aux (BDDPTR f)
{
  /* Mark wherever we go: */
  BDD_TOGGLE_MARK (f);

  if (BDD_X_P (f)) {
    BDD_AUX1_BDD (f) = bdd_1 ();
    return;
  }

  if (BDD_TERM_P (f)) {
    BDD_AUX1_BDD (f) = bdd_0 ();
    return;
  }

  {
    BDDPTR v;
    BDDPTR T = BDD_THEN (f);
    BDDPTR E = BDD_ELSE (f);

    if (BDD_NOT_MARKED (T, f))
      bdd_dontcare_set_aux (T);

    if (BDD_NOT_MARKED (E, f))
      bdd_dontcare_set_aux (E);

    v = bdd_create_var (BDD_VARID (f));
    T = dontcare_set_interpret_mod_bits (T);
    E = dontcare_set_interpret_mod_bits (E);

    BDD_AUX1_BDD (f) = bdd_ite (v, T, E);
    bdd_free (v);
    bdd_free (T);
    bdd_free (E);
  }
}

BDDPTR bdd_dontcare_set (BDDPTR f)
{
  int save_bdd_do_dynamic_ordering;
  BDDPTR R;

  if (BDD_VOID_P (f))
    return BDD_VOID;

  if (BDD_X_P (f))
    return bdd_1 ();

  if (BDD_TERM_P (f))
    return bdd_0 ();

  save_bdd_do_dynamic_ordering = bdd_do_dynamic_ordering;
  bdd_do_dynamic_ordering = 0;

  /* Here f non-constant. */
  bdd_dontcare_set_aux (f);
  R = dontcare_set_interpret_mod_bits (f);

  bdd_do_dynamic_ordering = save_bdd_do_dynamic_ordering;

  /* Also resets marks: */
  bdd_traverse_pre (f, bdd_free_aux1_action);

  return R;
}

static BDDPTR minimize_dontcares_interpret_mod_bits (BDDPTR f)
{
  BDDPTR R = BDD_AUX1_BDD (f);	/* !BDD_VOID_P (R) */

  if (BDD_I_INV_EDGE_P (f) && BDD_VARID (f) == BDD_VARID (R))
    /* R non-constant. Top-var of f might have disappeared in result R. */
    R = bdd_invert_input_top (R);
  else
    bdd_assign (R);

  if (BDD_O_INV_EDGE_P (f))
    /* Simply complement the negated-output bit: */
    R = BDD_COMPL (R);

  return R;
}

/* Pre: !BDD_VOID_P (f) */
static void bdd_minimize_dontcares_aux (BDDPTR f)
{
  /* Mark wherever we go: */
  BDD_TOGGLE_MARK (f);

  if (BDD_TERM_P (f)) {
    bdd_assign (f);
    BDD_AUX1_BDD (f) = PTR (f);
  }
  else {
    BDDPTR v;
    BDDPTR T = BDD_THEN (f);
    BDDPTR E = BDD_ELSE (f);

    if (BDD_NOT_MARKED (T, f))
      bdd_minimize_dontcares_aux (T);

    if (BDD_NOT_MARKED (E, f))
      bdd_minimize_dontcares_aux (E);

    T = minimize_dontcares_interpret_mod_bits (T);
    E = minimize_dontcares_interpret_mod_bits (E);

    if (BDD_X_P (T)) {
      bdd_free (T);

      BDD_AUX1_BDD (f) = E;
    }
    else
    if (BDD_X_P (E)) {
      bdd_free (E);

      BDD_AUX1_BDD (f) = T;
    }
    else { 
      /* Neither T nor E don't care. */
      v = bdd_create_var (BDD_VARID (f));

      BDD_AUX1_BDD (f) = bdd_ite (v, T, E);
      bdd_free (v);
      bdd_free (T);
      bdd_free (E);
    }
  }
}

/* Assumes f is a BDD with some X terminal nodes.
   This functions tries to minimize the BDD (decrease its size)
   by interpreting the X's either as 0's or 1's.
   The minimized BDD is returned; in particular: if f is BDD_VOID or
   BDD_0 or BDD_1 this is immediately returned. If f is BDD_X, BDD_0
   is returned. In general, when the choice to interpret an X is not
   obvious, it will always be interpreted as 0.
   It is guaranteed that the returned BDD has no X's.
   If f doesn't have X's, this function has no effect.
*/
BDDPTR bdd_minimize_dontcares (BDDPTR f)
{
  int save_bdd_do_dynamic_ordering;
  BDDPTR R;

  if (BDD_VOID_P (f))
    return BDD_VOID;

  if (BDD_X_P (f))
    return bdd_0 ();

  if (BDD_TERM_P (f))
    return bdd_assign (f);

  save_bdd_do_dynamic_ordering = bdd_do_dynamic_ordering;
  bdd_do_dynamic_ordering = 0;

  /* Here f non-constant. */
  bdd_minimize_dontcares_aux (f);
  R = minimize_dontcares_interpret_mod_bits (f);

  bdd_do_dynamic_ordering = save_bdd_do_dynamic_ordering;

  /* Also resets marks: */
  bdd_traverse_pre (f, bdd_free_aux1_action);

  if (BDD_X_P (R)) {
    bdd_free (R);
    return bdd_0 ();
  }
  return R;
}

/* Modifies f_vec by calling bdd_minimize_dontcares for each element.
   Original BDD elements are bdd_freed.
   Returns modified f_vec.
*/
BDDPTR *bdd_minimize_dontcares_vec (BDDPTR *f_vec, int size)
{
  int i;
  int save_bdd_do_dynamic_ordering;

  save_bdd_do_dynamic_ordering = bdd_do_dynamic_ordering;
  bdd_do_dynamic_ordering = 0;

  /* Assume all mark fields 0. */
  for (i = 0; i < size; i++) {
    BDDPTR f = f_vec[i];

    if (!BDD_VOID_P (f)) {

      if (BDD_MARK (f)) {
	BDDPTR R = minimize_dontcares_interpret_mod_bits (f);

	if (BDD_X_P (R)) {
	  bdd_free (R);
	  R = bdd_0 ();
	}

	bdd_free (f);
	f_vec[i] = R;
      }
      else
	bdd_minimize_dontcares_aux (f);
    }
  } /*for*/
  /* Now all mark fields are set. */

  for (i = 0; i < size; i++) {
    BDDPTR f = f_vec[i];

    if (!BDD_VOID_P (f) && BDD_MARK (f)) {
      BDDPTR R = minimize_dontcares_interpret_mod_bits (f);

      if (BDD_X_P (R)) {
	bdd_free (R);
	R = bdd_0 ();
      }

      /* Also resets marks: */
      bdd_traverse_pre (f, bdd_free_aux1_action);

      bdd_free (f);
      f_vec[i] = R;
    }
  }
  bdd_do_dynamic_ordering = save_bdd_do_dynamic_ordering;

  return f_vec;
}

/* Interpret the BDD's for D.f stored in the aux1 field of f
   according the modification bits of f.

   BDD_AUX1_BDD (f) = D.(PTR (f))

   pre: !BDD_VOID_P (f).
*/
#define diff_interpret_mod_bits(f) \
	dontcare_set_interpret_mod_bits (f)

/* pre: !BDD_VOID_P (f) && all vars in list have rank < BDD_TERMID
        && list != NULL

   Uses aux1 field to store partial D diff result;
   Note: D vars.(f') = D vars.f if vars != empty.

   Don't take modification bits of f into account, i.e.
   calculate result for PTR (f). Thus in the comments read PTR (f) for f.
   The final result is calculated by diff_interpret_mod_bits.
*/
static void bdd_diff_aux (BDDPTR f, BDD_ELEM vars)
{
  int topF = BDD_RANK (f);	/* f's top var rank number */
  int x;			/* the list element's variable */
  int rank;			/* its rank number */

  /* Mark wherever we go: */
  BDD_TOGGLE_MARK (f);

  /* D(x,y,z,...).Const = 0 */
  if (BDD_TERM_P (f)) {
    BDD_AUX1_BDD (f) = bdd_0 ();
    return;
  }

  /* BDD_INTERN_P (f) */

  /* id's might be negative to indicate negative literals.
     For our purposes consider them positive:
  */
  if ((x = BDD_ELEM_CONTENTS_I (vars)) < 0)
    x = -x;
  rank = BDD_VAR_RANK (x);

  /* Skip duplicate x's in vars: */
  for (; BDD_LIST_NEXT (vars); vars = BDD_LIST_NEXT (vars)) {
    int y;

    if ((y = BDD_ELEM_CONTENTS_I (BDD_LIST_NEXT (vars))) < 0)
      y = - y;
    if (y != x)
      break;
  } /*for*/

  /* Bottom case: */
  if (rank < topF) {
    /* x !elem sup(f) ==> D (x,y,z,...).f = 0 */
    BDD_AUX1_BDD (f) = bdd_0 ();
    return;
  }

  /* Here: topF <= rank < BDD_TERMID */
  {
    BDDPTR v, R1, R2;
    BDDPTR T = BDD_THEN (f);
    BDDPTR E = BDD_ELSE (f);

    if (rank == topF) {
      /*    f  =  x T  + x' E
	    f' = x T' + x' E'

	 D(x,y,z,...).f = Dx.(D(y,z,...).f)
	                = Dx.(D(y,z,...).(x T + x' E))
			= Dx.(D(y,z,...).(x T) + D(y,z,...).(x' E))
			= Dx.(x D(y,z,...).T + x' D(y,z,...).E)
			= D(y,z,...).T xor D(y,z,...).E

	 Note: x disappears in result.
      */
      if (BDD_LIST_NEXT (vars)) {
	if (BDD_NOT_MARKED (T, f))
	  bdd_diff_aux (T, BDD_LIST_NEXT (vars));

	if (BDD_NOT_MARKED (E, f))
	  bdd_diff_aux (E, BDD_LIST_NEXT (vars));

	R1 = diff_interpret_mod_bits (T); /* D(y,z,...).T */
	R2 = diff_interpret_mod_bits (E); /* D(y,z,...).E */
      }
      else {
	R1 = bdd_assign (BDD_COFACTOR_POS (f));
	R2 = bdd_assign (BDD_COFACTOR_NEG (f));
      }
      BDD_AUX1_BDD (f) = bdd_xor (R1, R2); /* D(x,y,z,...).f */
      bdd_free (R1);
      bdd_free (R2);
      return;
    }

    /* Here: rank > topF. */

    /* f  =  v T  + v' E
       f' =  v T' + v' E'

       rank(v) < rank(x) < rank(y) < rank(z) ...

       D(x,y,z,...).f = D(x,y,z,...).(v T + v' E)
		      = D(x,y,z,...).(v T) + D(x,y,z,...).(v' E)
		      = v D(x,y,z,...).T + v' D(x,y,z,...).E
		      = ite (v, D(x,y,z,...).T, D(x,y,z,...).E)

       Note: v might disappear in result.
    */

    /* Recursive case 2: */
    if (BDD_NOT_MARKED (T, f))
      /* Not yet been there, so recurse: */
      bdd_diff_aux (T, vars);

    if (BDD_NOT_MARKED (E, f))
      /* Not yet been there, so recurse: */
      bdd_diff_aux (E, vars);

    v = bdd_create_var (BDD_VARID (f));

    R1 = diff_interpret_mod_bits (T); /* D(x,y,z,...).T */
    R2 = diff_interpret_mod_bits (E); /* D(x,y,z,...).E */

    BDD_AUX1_BDD (f) = bdd_ite (v, R1, R2); /* D(x,y,z,...).f */
    bdd_free (R1);
    bdd_free (R2);
    bdd_free (v);
  }
}

/* Calculate D(vars).f, i.e., the boolean difference of f with respect to
   the variables that appear in the list vars. This list contains
   the id's of the variables in an increasing order according their
   rank numbers. It is allowed that a variable occurs more than once
   in the list (of course then they are consecutive occurrences).

   Definition: D x.f = f|x xor f|x'

   In particular for constant f's:
               D x.0 = 0
	       D x.1 = 0
	       D x.X = 0

   Uses the following identities:

   D(x1,x2,x3,...).f = D x1.(D(x2,x3,...).f)

   Let f = g', then
   D.f = D.g' = g'|x xor g'|x' = (g|x)' xor (g|x')' = g|x xor g|x' = D.g
   Thus, boolean difference is invariant under complementation.
*/
BDDPTR bdd_diff (BDDPTR f, BDD_LIST vars)
{
  int save_bdd_do_dynamic_ordering;
  BDDPTR R;

  if (BDD_VOID_P (f))
    return BDD_VOID;

  if (!vars)
    /* Empty list: interpret as no differencing. */
    return bdd_assign (f);

  if (BDD_TERM_P (f))
    return bdd_0 ();

  save_bdd_do_dynamic_ordering = bdd_do_dynamic_ordering;
  bdd_do_dynamic_ordering = 0;

  bdd_diff_aux (f, BDD_LIST_FIRST (vars));
  R = diff_interpret_mod_bits (f);

  bdd_do_dynamic_ordering = save_bdd_do_dynamic_ordering;

  /* Also resets marks: */
  bdd_traverse_pre (f, bdd_free_aux1_action);

  return R;
}

/* Struct that overlays AUX1 and AUX2 fields in BDD node.
   Used to record shortest path information:
   AUX1 records info about shortest path to a BDD_0 node;
   AUX2 records info about shortest path to a BDD_1 node.
   follow_then = 1 means must follow then link to get there,
   else must follow the else link.
   In case of ties the then link is preferred.
   length is length of shortest path counted in number of non-constant
   BDD nodes along it (>= 0), or BDD_MAXVARS + 1 when no shortest path exists.
*/
typedef struct {
  unsigned int follow_then0 :  1;
  unsigned int length0      : 31;
  unsigned int follow_then1 :  1;
  unsigned int length1      : 31;
} path_rec;

#define PATH_REC_CAST(v)	((path_rec *) &BDD_AUX (v))
#define FT0(v)			(PATH_REC_CAST(v)->follow_then0)
#define FT1(v)			(PATH_REC_CAST(v)->follow_then1)
#define LEN0(v)			(PATH_REC_CAST(v)->length0)
#define LEN1(v)			(PATH_REC_CAST(v)->length1)
#define INF			(BDD_MAXVARS + 1)

/* Determine shortest path to BDD_0 and BDD_1 nodes for each node.
   In fact records for each node which way to go to ultimately find
   the aimed constant and also the length of that path.
   Recorded info reflects case where edge to node is considered positive.
   Should therefore be interpreted the other way around for negative edges.
*/
static void bdd_shortest_path_action (BDDPTR v)
{
  if (BDD_TERM_P (v)) {
    LEN0 (v) = INF;
    LEN1 (v) = INF;

    /* If not using complemented edges we have a distinct BDD_0 and BDD_1
       node. In that case must make sure to record the proper path length
       values. For BDD_X nodes we always have INF paths to 0 and 1.
       If we do use complemented edges then only the info for BDD_1
       is recorded.
    */
    if (!BDD_X_P (v)) {
      if (bdd_use_neg_edges || BDD_1_P (v))
	LEN1 (v) = 0;
      else
	LEN0 (v) = 0;
    }
  }
  else {
    BDDPTR T = BDD_THEN (v);
    BDDPTR E = BDD_ELSE (v);
    unsigned int len0T = LEN0 (T);
    unsigned int len1T = LEN1 (T);
    unsigned int len0E = BDD_NEG_P (E) ? LEN1 (E) : LEN0 (E);
    unsigned int len1E = BDD_NEG_P (E) ? LEN0 (E) : LEN1 (E);

    if (len0T <= len0E) {
      /* Also captures the cases: 1. len0E == INF and/or 2. len0T == INF */
      FT0 (v) = 1;
      LEN0 (v) = len0T + (len0T < INF);
    }
    else {
      FT0 (v) = 0;
      LEN0 (v) = len0E + 1;
    }

    if (len1T <= len1E) {
      /* Also captures the cases: 1. len1E == INF and/or 2. len1T == INF */
      FT1 (v) = 1;
      LEN1 (v) = len1T + (len1T < INF);
    }
    else {
      FT1 (v) = 0;
      LEN1 (v) = len1E + 1;
    }
  }
}

/* Starting at node v follow shortest path to constant node indicated by
   to_1 (1 -> BDD_1, 0 -> BDD_0).
   Returns number of nodes found on this path, or INF when no such path
   exists.
   Calls action routine for each node along the path and also passes whether
   the node's variable is to be assigned 1 or 0 on that path.
   If path exists last call to action will have the constant node as argument.
*/
static unsigned int bdd_path_runner (BDDPTR f, void (*action) (BDDPTR, int),
				     int to_1)
{
  unsigned int len;

  /* If requesting a path to 1 but top edge is negative, then must in fact
     look for path to 0, and vice versa when looking for 0 path.
  */
  if (BDD_NEG_P (f))
    to_1 = !to_1;

  /* Get the length of the requested path: */
  len = to_1 ? LEN1 (f) : LEN0 (f);

  if (len < INF) {
    /* Path could have length 0, then f must be BDD_0 or BDD_1. */
    while (BDD_INTERN_P (f)) {
      unsigned int Tdir = to_1 ? FT1 (f) : FT0 (f);

      /* Determine sign (or phase) of variable in cube.
	 Take input inversion into account.
      */
      (*action) (f, Tdir == BDD_I_INV_EDGE_P (f));

      /* See which way shortest path proceeds: */
      f = Tdir ? BDD_THEN (f) : BDD_ELSE (f);
      /* Aim from now on depends on f's complement bit: */
      if (BDD_NEG_P (f))
	to_1 = !to_1;
    } /*while*/

    (*action) (f, to_1);
  }
  return len;
}

/* Global for collecting shortest path as a BDD_LIST of BDDPTRs. */
static BDD_LIST shortest_path_list;

/* Appending another node on the shortest path to the global list: */
static void collect_list_action (BDDPTR v, int neg)
{
  shortest_path_list = bdd_list_append_cont (v, shortest_path_list);
}

/* Determine the shortest satisfying path in f as a BDD_LIST of BDDPTRs.
   Does not take BDD_Xs into account.
   Returns empty list if no such path exists, else list is terminated
   by trailing constant BDD node.
*/
BDD_LIST bdd_shortest_path_to_1_as_list (BDDPTR f)
{
  if (BDD_VOID_P (f))
    return BDD_LIST_NULL;

  shortest_path_list = BDD_LIST_NULL;

  bdd_traverse_post (f, bdd_shortest_path_action);

  bdd_path_runner (f, collect_list_action, 1);

  bdd_traverse_pre (f, bdd_reinit_aux1_and_aux2_action);

  return shortest_path_list;
}

/* Global for constructing cube for shortest path. */
static BDDPTR shortest_path_cube;

/* Add another literal to the shortest path cube. */
static void collect_cube_action (BDDPTR v, int neg)
{
  if (BDD_INTERN_P (v)) {
    BDDPTR var = bdd_create_var (BDD_VARID (v));
    BDDPTR tmp;

    if (neg) {
      /* This node's variable appears negated in cube: */
      tmp = bdd_not (var);
      bdd_free (var);
      var = tmp;
    }

    /* Add another variable to the cube: */
    tmp = bdd_and (shortest_path_cube, var);
    bdd_free (shortest_path_cube);
    bdd_free (var);
    shortest_path_cube = tmp;
  }
}

/* Determine the shortest satisfying (to_1 = 1) or contradicting (to_1 = 0)
   path in f. len if non-zero is address of int variable in which the
   path length will be written.
   If f is BDD_VOID returns BDD_VOID and *len is undefined.
   Otherwise, returns BDD cube representing the path if one indeed exists,
   or BDD_0 to indicate non-existence (*len will be BDD_MAXVARS + 1 then).
   Note that when a contradicting path is requested and f is BDD_0 then
   the result will be the BDD_1 cube, i.e., every assignment to the
   variables will make f 0, and the path length is 0.
*/
BDDPTR bdd_shortest_path_as_cube (BDDPTR f, int to_1, int *len)
{
  int save_bdd_do_dynamic_ordering;
  int path_len;

  if (BDD_VOID_P (f))
    return BDD_VOID;

  bdd_traverse_post (f, bdd_shortest_path_action);

  shortest_path_cube = bdd_1 ();

  save_bdd_do_dynamic_ordering = bdd_do_dynamic_ordering;
  bdd_do_dynamic_ordering = 0;

  path_len = bdd_path_runner (f, collect_cube_action, to_1);

  bdd_do_dynamic_ordering = save_bdd_do_dynamic_ordering;

  if (len) *len = (int) path_len;
  if (path_len == INF) {
    bdd_free (shortest_path_cube);
    shortest_path_cube = bdd_0 ();
  }

  bdd_traverse_pre (f, bdd_reinit_aux1_and_aux2_action);

  return shortest_path_cube;
}

BDDPTR bdd_shortest_path_to_1_as_cube (BDDPTR f)
{
  return bdd_shortest_path_as_cube (f, 1, 0);
}

/* Some notes on BDDs and sum-of-product representations.

   Consider BDD: f = <v, T, E>
 
   Task: derive a sum-of-products representation for f.
   Implement using set (BDD_LIST) of BDD's, each BDD representing 1 cube.
   Need to calculate and keep such a set for each node in the BDD argument.
   Could store this set in the BDD data structure, except when using
   negative edges, then must store 2 sets. Might also consider sharing the
   elements of several sets.
   Calculation proceeds recursively as follows:

   0-node gets the empty set, 0-set = {},
   X-node gets the empty set, 0-set = {},
   1-node gets the singleton set with the 1 cube in it, 1-set = {1},
   for a node v with set for then-edge T_sop, and set for else-edge E_sop:

   if E covers a cube in T_sop then include this cube in result
   else include v.cube in result;
   if T covers a cube in E_sop then include this cube in result
   else include v'.cube in result.

   Some useful definitions:
   A function is monotone non-decreasing in a variable v iff a change
   of v from 0 to 1 can under no circumstances (assignment to variables
   other than v) result in a change of f from 1 to 0.
   Let f|v=1 be the cofactor of f under the assignment v = 1 and
   let f|v=0 be the cofactor of f under the assignment v = 0, then
   f is monotone non-decreasing in v iff f|v=0 . (f|v=1)' = 0
   (= 0 means "not satisfiable"). [This is equivalent to requiring that
   f|v=0 implies f|v=1 is a tautology]

   A function is monotone non-increasing in a variable v iff a change
   of v from 0 to 1 can under no circumstances (assignment to variables
   other than v) result in a change of f from 0 to 1.
   f is monotone non-increasing in v iff (f|v=0)' . f|v=1 = 0.
   [Equivalent to f|v=1 implies f|v=0]

   A function is monotone or unate in a variable v iff either f is
   monotone non-decreasing in v or f is monotone non-increasing in v.
   For variables that are not in the support of f both conditions hold
   simultaneously, otherwise exactly one condition holds for a unate variable.

   A function is unate iff it is unate in all its (support) variables.

   A function that is not unate in a variable is said to be binate in that
   variable. A function is binate iff it is not unate.

   Note: F = 0 can be written as F' = 1 (A function F is not satisfiable
   is equivalent with saying that the negation of F is a tautology).
   So, f|v=0 . (f|v=1)' = 0 is equivalent with (f|v=0)' + f|v=1 = 1,
   or better (f|v=0 -> f|v=1) = 1.

   In a BDD f = < v, T, E > (read f = v.T + v'.E), f is unate in v iff
   T -> E = 1 or E -> T = 1, f is binate in v otherwise.

   A function f covers a function h, denoted f >= h, when h implies f,
   i.e. f assumes 1 whenever h does. Our notation: (f <- h) = 1, (h -> f) = 1
   (= 1 means "is a tautology").
   If h is a product of literals and (h -> f) = 1, then h is called an
   implicant.

   A prime implicant p of a function f is an implicant with a minimal set
   of literals: deleting any one of them causes the resulting product term
   to no longer imply f.

   A sum-of-products expression for a function f is said to be irredundant
   when any attempt to reduce it either by deleting a term (product) or a
   literal (within a term) will yield an expression that is not equivalent
   to f.
*/

int bdd_unate_in (BDDPTR f, int var)
{
  int rank = BDD_VAR_RANK (var);

  if (BDD_VOID_P (f) || rank == BDD_TERMID)
    return 0;

  if (rank < BDD_RANK (f))
    /* Covers the case that BDD_TERM_P (f). */
    return 1;

  /* Here: rank >= BDD_RANK (f) */
  {
    BDDPTR T, E;
    int result;
    int save_bdd_do_dynamic_ordering;

    save_bdd_do_dynamic_ordering = bdd_do_dynamic_ordering;
    bdd_do_dynamic_ordering = 0;

    T = bdd_cofactor_pos (f, var);
    E = bdd_cofactor_neg (f, var);
    result = BDD_COVERS (T, E) || BDD_COVERS (E, T);
    bdd_free (T);
    bdd_free (E);

    bdd_do_dynamic_ordering = save_bdd_do_dynamic_ordering;

    return result;
  }
}

/* Does free the arguments T_sop and E_sop. */
/* Assumes no multiple occurrences of cubes in T_sop and the same for E_sop.
   Also, T_sop and E_sop contain prime implicants for T, resp. E.
   However, the same cube might occur in both!

   Returns sop for f = var.T + var'.E given T_sop and E_sop.
   Result is guaranteed not to contain multiple occurrences of the same cube.
   All cubes in the result are prime.
*/
static BDD_LIST prime_implicants (int var, BDDPTR T, BDD_LIST T_sop,
				  BDDPTR E, BDD_LIST E_sop)
{
  BDDPTR top, top_neg;
  BDD_LIST result = BDD_LIST_NULL;

  /* Create BDD for var (positive literal): */
  top = bdd_create_var (var);
  /* Create BDD for complement of var (negative literal): */
  top_neg = bdd_not (top);

  /* case 1: T -> E = 1, then f = <v,T,E> unate in v, E'.T = 0;
     	f = v.T + v'.E = v.T + v'.(E + E'.T) = v.T + v'.(E + T)
	  = T + v'.E = T + v'.T'.E
     case 2: E -> T = 1, then f = <v,T,E> unate in v, E.T' = 0;
     	f = v.T + v'.E = v.(T + E.T') + v'.E = v.(T + E) + v'.E
	  = v.T + E = v.T.E' + E
     case 3:  otherwise, then f = <v,T,E> binate in v.
  */

  /* Not sure whether considering 3 cases is more efficient.
     For now cases 1 and 2 are also covered by the case 3 code.
  */
#ifdef COMMENT
  if (BDD_COVERS (E, T)) {	/* Case 1 */
    E_sop = set_difference (E_sop, T_sop);
    E_sop = include_lit (top_neg, E_sop);
    bdd_free (top);
    return disjoint_union (T_sop, E_sop);
  }

  if (BDD_COVERS (T, E)) {	/* Case 2 */
    T_sop = set_difference (T_sop, E_sop);
    T_sop = include_lit (top, T_sop);
    bdd_free (top_neg);
    return disjoint_union (T_sop, E_sop);
  }
#endif
				/* Case 3 */
  while (T_sop) {
    BDDPTR cube = bdd_list_pop_cont (&T_sop);

    if (!BDD_COVERS (E, cube)) {
      /* cube is not expandable in `top' direction.
	 Must create new cube: top AND cube.
      */
      BDDPTR R;

      R = bdd_and (top, cube);
      bdd_free (cube);
      cube = R;
    }
    result = bdd_list_append_cont (cube, result);
  } /*while*/
  bdd_free (top);

  while (E_sop) {
    BDDPTR cube = bdd_list_pop_cont (&E_sop);

    if (!BDD_COVERS (T, cube)) {
      /* cube is not expandable in `top_neg' direction.
	 Must create new cube: top_neg AND cube.
      */
      BDDPTR R;

      R = bdd_and (top_neg, cube);
      bdd_free (cube);
      /* Cube R for sure not yet in result. */
      result = bdd_list_append_cont (R, result);
    }
    else {
      /* Mind that cube could already be in result. */
      if (bdd_list_present (cube, result, 0 /* means test is == */))
	bdd_free (cube);
      else
	result = bdd_list_append_cont (cube, result);
    }
  } /*while*/
  bdd_free (top_neg);

  return result;
}

/* Flag that controls whether sop_cache is being used. */
static int bdd_use_sop_cache = 1;

void bdd_use_sop_cache_switch (int on)
{
  if (on)
    bdd_use_sop_cache = 1;
  else {
    /* Might as well clean up the thing: */
    bdd_cleanup_sop_cache ();
    bdd_use_sop_cache = 0;
  }
}

/* Must be prime number. With 1279, spending ~10kbytes on cache. */
#define SOP_CACHE_SIZE	1279

typedef struct sop_cache_entry SOP_CACHE_ENTRY;

/* Per entry: sizeof (BDDPTR) + sizeof (BDD_LIST) = 8 bytes. */
static struct sop_cache_entry {
  BDDPTR f;
  BDD_LIST R;
} sop_cache[SOP_CACHE_SIZE] = {0};

static int nr_hits = 0;
static int nr_lookups = 0;
static int nr_collisions = 0;
static int occupancy = 0;

#define hash_sop(a) \
  ((((int) a) & INT_MAX) % SOP_CACHE_SIZE)

/* Looks up BDD f in sop_cache and when a sum-of-cubes list is present
   for it returns a full copy of the list (list elements are BDD cubes
   and these are bdd_assign-ed).
   If use of cache is switched off immediately returns the BDD_LIST_NULL.
*/
static BDD_LIST lookup_sop_cache (BDDPTR f)
{
  if (bdd_use_sop_cache) {
    BDD_LIST R;
    SOP_CACHE_ENTRY *entry = sop_cache + hash_sop (f);

    nr_lookups++;

    if ((R = entry->R) && BDD_EQUAL_P (f, entry->f)) {
      nr_hits++;
      return bdd_list_copy (R, (void *(*)(void *)) bdd_assign);
    }
  }
  return BDD_LIST_NULL;
}

/* Records the pair (f, R) in the sop cache.
   Overwrites any existing info for f.
   Copies of both arguments are made to store in the cache.
*/
static void insert_sop_cache (BDDPTR f, BDD_LIST R)
{
  SOP_CACHE_ENTRY *entry;

  if (!bdd_use_sop_cache)
    return;

  entry = sop_cache + hash_sop (f);

  /* Let's be honest, check for equal data: */
  if (entry->R && BDD_EQUAL_P (f, entry->f))
    /* Already stored. */
    return;

  bdd_assign (f);
  if (entry->R) {
    nr_collisions++;
    bdd_list_free (entry->R, (void (*)(void *)) bdd_free);
    bdd_free (entry->f);
  }
  else
    /* A fresh entry: */
    occupancy++;
  entry->f = f;
  entry->R = bdd_list_copy (R, (void *(*)(void *)) bdd_assign);
}

void bdd_cleanup_sop_cache (void)
{
  register int size = SOP_CACHE_SIZE;
  register SOP_CACHE_ENTRY *entry = sop_cache;

  if (!occupancy)
    return;

  while (size--) {
    if (entry->R) {
      bdd_list_free (entry->R, (void (*)(void *)) bdd_free);
      bdd_free (entry->f);
      /* Indicate entry is empty: */
      entry->R = BDD_LIST_NULL;
      /* Just for safety: */
      entry->f = BDD_VOID;
    }
    entry++;
  }
  nr_hits = 0;
  nr_lookups = 0;
  nr_collisions = 0;
  occupancy = 0;
}

/* Note: using cache instead of aux fields and marking to avoid visiting
   same node too often.
*/
static BDD_LIST sum_of_cubes_as_list_aux (BDDPTR f)
{
  if (BDD_0_P (f) || BDD_X_P (f))
    /* Must interpret X's as 0's because cannot handle them in cubes. */
    return BDD_LIST_NULL;

  if (BDD_TERM_P (f))		/* Found a satisfying path */
    /* Return singleton list containing the 1 cube: */
    return bdd_list_push_cont (bdd_assign (f), BDD_LIST_NULL);

  /* Extra speed up for literal BDD's: */
  if (BDD_LIT_P (f))
    /* Return singleton list containing f: */
    return bdd_list_push_cont (bdd_assign (f), BDD_LIST_NULL);

  /* Here f is a non-constant node. */
  {
    BDD_LIST result;

    if (!(result = lookup_sop_cache (f))) {
      /* Note that T and E will usually be BDD_GC_PROTECT-ed when f is;
	 Except when dynamic variable ordering occurs e.g. in the
	 following recursive calls or the call to prime_implicants.
	 In that case T and E may get freed, therefore it is crucial
	 to protect them here! Of course we could also have chosen
	 to disable dynamic variable ordering throughout this routine.
	 Note also that v should be determined prior to any recursive
	 calls; again this is because of dynamic variable ordering which
	 might change the BDD_VARID of f. The effect might then be that
	 the wrong variable is used with a result from the sop_cache
	 (stored there before the variable ordering took place).
      */
      int    v = BDD_VARID (f);
      BDDPTR T = bdd_assign (BDD_COFACTOR_POS (f));
      BDDPTR E = bdd_assign (BDD_COFACTOR_NEG (f));
      BDD_LIST T_sop, E_sop;

      /* Recurse for subgraphs: */
      T_sop = sum_of_cubes_as_list_aux (T);
      E_sop = sum_of_cubes_as_list_aux (E);

      /* prime_implicants frees the T_sop and E_sop arguments. */
      result = prime_implicants (v, T, T_sop, E, E_sop);

      bdd_free (T);
      bdd_free (E);

      insert_sop_cache (f, result);
    }

    /* Here result is list of distinct prime implicants for f. */
    return result;
  }
}

BDD_LIST bdd_sum_of_cubes_as_list (BDDPTR f)
{
  int save_bdd_do_dynamic_ordering;
  BDD_LIST R;

  if (BDD_VOID_P (f))
    return BDD_LIST_NULL;

  save_bdd_do_dynamic_ordering = bdd_do_dynamic_ordering;
  /* This is optional, but since only BDD cubes are constructed there is
     no point in doing any reorderings.
  */
  bdd_do_dynamic_ordering = 0;

  R = sum_of_cubes_as_list_aux (f);
/*  bdd_cleanup_sop_cache ();*/

  bdd_do_dynamic_ordering = save_bdd_do_dynamic_ordering;

  return R;
}

/* Returns BDD representation for the function described by the sum of
   cubes leaving out cube.
*/
static BDDPTR list_without_cube_as_bdd (BDD_LIST cubes, BDDPTR cube)
{
  BDD_LIST list2;
  BDDPTR R1, R2, F;

  if (BDD_LIST_SIZE (cubes) == 0)
    return bdd_0 ();

  if (BDD_LIST_SIZE (cubes) == 1) {
    BDDPTR cube1 = BDD_ELEM_CONTENTS (BDD_LIST_FIRST (cubes));

    if (BDD_EQUAL_P (cube1, cube))
      return bdd_0 ();
    return bdd_assign (cube1);
  }

  /* Here: BDD_LIST_SIZE (cubes) > 1 */
  list2 = bdd_list_bisect (cubes);
  R1 = list_without_cube_as_bdd (cubes, cube);
  R2 = list_without_cube_as_bdd (list2, cube);
  F = bdd_or (R1, R2);
  bdd_free (R1);
  bdd_free (R2);
  /* Reconstruct list: */
  bdd_list_concat (cubes, list2);

  return F;
}

BDD_LIST bdd_irredundant_sum_of_cubes_as_list (BDDPTR f)
{
  int save_bdd_do_dynamic_ordering;
  BDD_LIST cubes, result;
  BDDPTR R;

  if (BDD_VOID_P (f))
    return BDD_LIST_NULL;

  save_bdd_do_dynamic_ordering = bdd_do_dynamic_ordering;
  bdd_do_dynamic_ordering = 0;

  cubes  = bdd_sum_of_cubes_as_list (f);
  result = BDD_LIST_NULL;

  /* Here cubes is list of prime implicants for f. */

  /* Check for multiple cube containment: */
  BDD_LIST_FOR_EACH_ELEM (cubes, elem) {
    BDDPTR cube = BDD_ELEM_CONTENTS (elem);

    /* Create R = cubes \ { cube }: */
    R = list_without_cube_as_bdd (cubes, cube);

    /* There are 2 ways to determine whether cube is redundant:
       1. R covers the cube,
       2. R is equal to the original function f.
       The latter is a faster test.
    */
    if (BDD_EQUAL_P (R, f)) {
      /* Redundant cube found. Must remove it from the list: */
      bdd_free (cube);
      /* No real delete; just put a harmless BDD_0 element: */
      BDD_ELEM_SET_CONTENTS (elem, bdd_0 ());
    }
    else
      result = bdd_list_append_cont (bdd_assign (cube), result);
    bdd_free (R);
  } BDD_LIST_END_FOR_EACH_ELEM;
  bdd_list_free (cubes, (void (*)(void *)) bdd_free);

#ifdef COMMENT
  /* Check: */
  R = sum_of_cubes_as_bdd (result);

  if (!BDD_EQUAL_P (R, f))
   fprintf (stderr, "SEVERE ERROR: irredundant_sum_of_cubes_as_list fails.\n");
  bdd_free (R);
#endif

  bdd_do_dynamic_ordering = save_bdd_do_dynamic_ordering;

  return result;
}

int bdd_traverse_cube (BDDPTR cube,
		       void (*action) (int index, int neg, int first))
{
  int cube_p = 1;
  int first = 1;

  if (BDD_VOID_P (cube) || BDD_X_P (cube))
    /* Not a cube. No further action. */
    return 0;

  if (BDD_TERM_P (cube))
    /* No further action. */
    return 1;

  /* Here cube is non-constant. */
  do {
    BDDPTR T = BDD_COFACTOR_POS (cube);
    BDDPTR E = BDD_COFACTOR_NEG (cube);
    int index = BDD_VARID (cube);

    /* In a valid bdd cube each node has at least 1 constant child,
       and if only 1 then it must be the BDD_0.
       The bottom node has 2 constant children and they must be
       BDD_0 and BDD_1.
       There may not be any X's in a cube.
       To avoid an endless loop in a non-cube argument, we break the tie by
       taking the then-edge in case of invalid nodes.
    */

    if (BDD_0_P (E)) {
      /* T can be either 1, X, or non-constant. */
      /* Must assign 1 to variable to reach T: */
      (*action) (index, 0, first);
      first = 0;
      if (BDD_TERM_P (T))
	return cube_p && BDD_1_P (T);
      cube = T;
    }
    else
    if (BDD_0_P (T)) {
      /* E can be either 1, X, or non-constant. */
      /* Must assign 0 to variable to reach E: */
      (*action) (index, 1, first);
      first = 0;
      if (BDD_TERM_P (E))
	return cube_p && BDD_1_P (E);
      cube = E;
    }
    else {
      /* Both successors are non-0. Take then-edge: */
      cube_p = 0;
      /* T can be either 1, X, or non-constant. */
      /* Must assign 1 to variable to reach T: */
      (*action) (index, 0, first);
      first = 0;
      if (BDD_TERM_P (T))
	return 0;
      cube = T;
    }
  } while (1);
}

static int lit_count;
static void count_lit_action (int v, int neg, int first)
{
  lit_count++;
}

int bdd_size_cube (BDDPTR cube)
{
  if (BDD_VOID_P (cube) || BDD_TERM_P (cube))
    return 0;

  lit_count = 0;
  bdd_traverse_cube (cube, count_lit_action);
  return lit_count;
}

static int bdd_cube_length (BDDPTR cube)
{
  if (BDD_0_P (cube))
    return BDD_TERMID;

  {
    BDDPTR next;
    int length = 0;

    while (!BDD_1_P (cube)) {
      length++;

      next = BDD_COFACTOR_POS (cube);
      if (BDD_0_P (next))
	next  = BDD_COFACTOR_NEG (cube);
      cube = next;
    }

    return length;
  }
}

/* Interpret the BDD's for prime implicant results stored in the aux fields
   of f according the modification bits of f.

   BDD_AUX1_BDD (f) = prime_implicant (PTR (f))
   BDD_AUX2_BDD (f) = prime_implicant (!PTR (f))

   Note: aux2 field only necessary when bdd_use_neg_edges = 1.
   Result is protected.
   pre: !BDD_VOID_P (f).
*/
static BDDPTR prime_implicant_interpret_mod_bits (BDDPTR f)
{
  BDDPTR R = BDD_O_INV_EDGE_P (f) ? BDD_AUX2_BDD (f) : BDD_AUX1_BDD (f);

  if (BDD_I_INV_EDGE_P (f) && BDD_VARID (f) == BDD_VARID (R))
    /* R non-constant. Top-var of f might have disappeared in result R. */
    R = bdd_invert_input_top (R);
  else
    bdd_assign (R);

  return R;
}

static void bdd_prime_implicant_aux (BDDPTR f)
{
  /* Mark wherever we go: */
  BDD_TOGGLE_MARK (f);

  if (BDD_TERM_P (f)) {
    if (BDD_X_P (f))
      BDD_AUX1_BDD (f) = bdd_0 ();
    else
      BDD_AUX1_BDD (f) = bdd_assign (PTR (f));

    if (bdd_use_neg_edges) {
      if (BDD_X_P (f))
	BDD_AUX2_BDD (f) = bdd_0 ();
      else
	BDD_AUX2_BDD (f) = bdd_not (BDD_AUX1_BDD (f));
    }
    return;
  }

  {
    BDDPTR v;
    BDDPTR T = BDD_THEN (f);
    BDDPTR E = BDD_ELSE (f);
    BDDPTR v_not;
    BDDPTR cex_T;
    BDDPTR cex_E;

    if (BDD_NOT_MARKED (T, f))
      bdd_prime_implicant_aux (T);

    if (BDD_NOT_MARKED (E, f))
      bdd_prime_implicant_aux (E);

    v = bdd_create_var (BDD_VARID (f));
    v_not = bdd_not (v);

    cex_T = prime_implicant_interpret_mod_bits (T);
    cex_E = prime_implicant_interpret_mod_bits (E);

    if (!BDD_IMPLIES_TAUT (cex_T, E)) {
      BDDPTR tmp = cex_T;

      cex_T = bdd_and (tmp, v);
      bdd_free (tmp);
    }

    if (!BDD_IMPLIES_TAUT (cex_E, T)) {
      BDDPTR tmp = cex_E;

      cex_E = bdd_and (tmp, v_not);
      bdd_free (tmp);
    }

    if (bdd_cube_length (cex_T) <= bdd_cube_length (cex_E)) {
      BDD_AUX1_BDD (f) = cex_T;
      bdd_free (cex_E);
    }
    else {
      BDD_AUX1_BDD (f) = cex_E;
      bdd_free (cex_T);
    }

    if (bdd_use_neg_edges) {
      T = BDD_COMPL (T);
      E = BDD_COMPL (E);

      cex_T = prime_implicant_interpret_mod_bits (T);
      cex_E = prime_implicant_interpret_mod_bits (E);

      if (!BDD_IMPLIES_TAUT (cex_T, E)) {
	BDDPTR tmp = cex_T;

	cex_T = bdd_and (tmp, v);
	bdd_free (tmp);
      }

      if (!BDD_IMPLIES_TAUT (cex_E, T)) {
	BDDPTR tmp = cex_E;

	cex_E = bdd_and (tmp, v_not);
	bdd_free (tmp);
      }

      if (bdd_cube_length (cex_T) <= bdd_cube_length (cex_E)) {
	BDD_AUX2_BDD (f) = cex_T;
	bdd_free (cex_E);
      }
      else {
	BDD_AUX2_BDD (f) = cex_E;
	bdd_free (cex_T);
      }
    }
    bdd_free (v);
    bdd_free (v_not);
  }
}

/* Computes a relatively small (in the sense of number of literals)
   prime implicant for f.
   Returns this as a BDD cube.
   Immediately returns BDD_VOID for BDD_VOID argument.
*/
BDDPTR bdd_prime_implicant (BDDPTR f)
{
  BDDPTR result;
  int save_bdd_do_dynamic_ordering;

  if (BDD_VOID_P (f))
    return BDD_VOID;

  save_bdd_do_dynamic_ordering = bdd_do_dynamic_ordering;
  bdd_do_dynamic_ordering = 0;

  bdd_prime_implicant_aux (f);

  result = prime_implicant_interpret_mod_bits (f);

  bdd_do_dynamic_ordering = save_bdd_do_dynamic_ordering;

  /* Also resets marks: */
  if (bdd_use_neg_edges)
    bdd_traverse_pre (f, bdd_free_aux1_and_aux2_action);
  else
    bdd_traverse_pre (f, bdd_free_aux1_action);

  return result;
}

BDDPTR bdd_eval (BDDPTR f, BDDPTR cube)
{
  BDDPTR look_for = BDD_1;	/* Initially, looking for BDD_1 */
  int negate = 0;

  if (BDD_VOID_P (f))
    return BDD_VOID;

  if (BDD_TERM_P (f))
    return bdd_assign (f);

  if (BDD_VOID_P (cube))
    return BDD_VOID;;

  if (BDD_X_P (cube))
    return bdd_assign (cube);

  if (BDD_0_P (cube)) {
    while (BDD_INTERN_P (f)) {
      negate ^= BDD_NEG_P (f);
      /* Assign 0 to all variables: */
      f = BDD_I_INV_EDGE_P (f) ? BDD_THEN (f) : BDD_ELSE (f);
    }
    bdd_assign (f);
    return negate ? BDD_COMPL (f) : f;
  }

  if (BDD_1_P (cube)) {
    while (BDD_INTERN_P (f)) {
      negate ^= BDD_NEG_P (f);
      /* Assign 1 to all variables: */
      f = BDD_I_INV_EDGE_P (f) ? BDD_ELSE (f) : BDD_THEN (f);
    }
    bdd_assign (f);
    return negate ? BDD_COMPL (f) : f;
  }

  /* Here cube is non-constant. */
  do {
    BDDPTR T, E;
    int index = BDD_RANK (cube);

    /* Set all variables till the one with index to 0: */
    while (BDD_INTERN_P (f) && BDD_RANK (f) != index) {
      negate ^= BDD_NEG_P (f);
      /* Assign 0 to variable: */
      f = BDD_I_INV_EDGE_P (f) ? BDD_THEN (f) : BDD_ELSE (f);
    }

    /* Toggle what we're looking for when traversing complemented edge: */
    if (BDD_NEG_P (cube))
      look_for = BDD_COMPL (look_for);

    /* In case of cube being an inverted input edge, the roles of T and E
       should be swapped:
    */
    if (BDD_I_INV_EDGE_P (cube)) {
      E = BDD_THEN (cube);
      T = BDD_ELSE (cube);
    }
    else {
      T = BDD_THEN (cube);
      E = BDD_ELSE (cube);
    }

    /* Is E the constant we're looking for? */
    if (BDD_TERM_P (E)) {
      if (BDD_EQUAL_P (E, look_for)) {
	/* Must set variable to 0 to reach it: */
	/* Assign 0 to rest of variables: */
	do {
	  negate ^= BDD_NEG_P (f);
	  /* Assign 0 to variable: */
	  f = BDD_I_INV_EDGE_P (f) ? BDD_THEN (f) : BDD_ELSE (f);
	} while (BDD_INTERN_P (f));
	bdd_assign (f);
	return negate ? BDD_COMPL (f) : f;
      }
      /* Must continue via T edge (or that's the const we want). */
      /* Must set variable to 1 to reach it: */
      if (BDD_INTERN_P (f)) {
	negate ^= BDD_NEG_P (f);
	/* Assign 1 to variable: */
	f = BDD_I_INV_EDGE_P (f) ? BDD_ELSE (f) : BDD_THEN (f);
      }
    }
    else {
      /* Must continue via E edge, expect T to be a constant. */
      /* Must set variable to 0 to reach it: */
      if (BDD_INTERN_P (f)) {
	negate ^= BDD_NEG_P (f);
	/* Assign 0 to variable: */
	f = BDD_I_INV_EDGE_P (f) ? BDD_THEN (f) : BDD_ELSE (f);
      }
    }

    /* Act as if E is a constant to avoid endless loop, i.e. for invalid
       cube nodes its variable is taken to appear positive.
    */
    cube = T;

    /* Is T the constant we're looking for? */
    if (BDD_TERM_P (T)) {
      if (BDD_EQUAL_P (T, look_for)) {
	/* Already did the action, ready now: */
	while (BDD_INTERN_P (f)) {
	  negate ^= BDD_NEG_P (f);
	  /* Assign 0 to rest of variables: */
	  f = BDD_I_INV_EDGE_P (f) ? BDD_THEN (f) : BDD_ELSE (f);
	}
	bdd_assign (f);
	return negate ? BDD_COMPL (f) : f;
      }
      /* Must continue via E edge: */
      cube = E;
    }

    if (BDD_X_P (cube))
      return bdd_assign (cube);

  } while (1);
}

#ifdef COMMENT

#define BDD_CUBE_TOP_POS_LIT_P(c) \
                                BDD_0_P (BDD_COFACTOR_NEG (c))

#define BDD_CUBE_NEXT(c)	(BDD_CUBE_TOP_POS_LIT_P (c) \
                                  ? BDD_COFACTOR_POS (c) \
                                  : BDD_COFACTOR_NEG (c))

static BDDPTR bdd_cube_factor_aux (BDDPTR f, BDDPTR cube)
{
  BDDPTR R;
  BDDPTR next_cube = cube;
  BDDPTR tmp;

  if (BDD_TERM_P (f))
    next_cube = BDD_1_P (f) ? BDD_1 : cube;
  else
    /* Skip all literals in next_cube that are skipped by edge f. */
    while (BDD_RANK (next_cube) < BDD_RANK (f))
      next_cube = BDD_CUBE_NEXT (next_cube);
    /* BDD_RANK (next_cube) >= BDD_RANK (f) */

  if (BDD_MARK (f)) {
    if (BDD_TERM_P (f) || BDD_NEG_P (f) == BDD_AUX1_L (f))
      return bdd_assign (next_cube);
    return bdd_1 ();
  }

  BDD_TOGGLE_MARK (f);
  BDD_AUX1_L (f) = BDD_NEG_P (f);

  if (BDD_TERM_P (f))
    R = bdd_assign (next_cube);
  else {
    BDDPTR T = BDD_COFACTOR_POS (f);
    BDDPTR E = BDD_COFACTOR_NEG (f);
    int var_phase   = BDD_CUBE_TOP_POS_LIT_P (next_cube);
    int var_in_cube = BDD_RANK (f) == BDD_RANK (next_cube);

    if (var_in_cube)
      next_cube = BDD_CUBE_NEXT (next_cube);

    tmp = bdd_cube_factor_aux (T, next_cube);
    R   = bdd_cube_factor_aux (E, tmp);
    bdd_free (tmp);

    if (   BDD_0_P (cube) && (BDD_0_P (T) || BDD_0_P (E))
	|| var_in_cube && (var_phase ? BDD_0_P (E) : BDD_0_P (T))) {
      BDDPTR v = bdd_create_var (BDD_VARID (f));

      tmp = R;
      if (BDD_0_P (T))
	R = bdd_ite (v, BDD_0, tmp);
      else
	R = bdd_ite (v, tmp, BDD_0);      
      bdd_free (v);
      bdd_free (tmp);
    }
  }
  return R;
}

BDDPTR bdd_cube_factor (BDDPTR f)
{
  BDDPTR R;
  int save_bdd_do_dynamic_ordering;

  if (BDD_VOID_P (f))
    return BDD_VOID;

  save_bdd_do_dynamic_ordering = bdd_do_dynamic_ordering;
  bdd_do_dynamic_ordering = 0;

  R = bdd_cube_factor_aux (f, BDD_0);
  bdd_traverse_pre (f, bdd_reinit_aux1_action);
/*  bdd_reset_marks (f);*/

  bdd_do_dynamic_ordering = save_bdd_do_dynamic_ordering;

  return R;
}
#endif

/* Definition of cube factor CF (f):

   CF (0) = 0,
   CF (1) = 1,
   CF (X) = 1,
   CF (v) = v,
   CF (v T) = v CF (T),
   CF (v' E) = v' CF (E),
   CF (v T + v' E) = CF (CF (T) + CF (E)) = CF (T + E).

   When f is expressed as a sum of cubes, the cube factor may be interpreted
   as the so-called largest common-cube (large in terms of #literals).
   We will call g = f / c, with c = CF (f), the quotient.

   f = CF (f) g.
   g = f|CF(f) = E CF(f) . f

   f = 0 => g = 0
   f = 1 => g = 1
   f = X => g = X
   f = v => g = 1
*/

/* Simple-minded approach to implement bdd_cube_factor.
   Worst-case would require a number recursions and also bdd_or operations
   equal to the depth of the original BDD f.
   Note that the bdd_or is each time performed on the 2 cofactors of f.
   Adding a new literal to the partial result will be very fast:
   the literal will always become the top-var of the new result.
*/
BDDPTR bdd_cube_factor (BDDPTR f)
{
  /* First deal with the bottom cases: */
  if (BDD_VOID_P (f))
    return BDD_VOID;

  if (BDD_0_P (f))
    return bdd_0 ();

  if (BDD_TERM_P (f))
    /* Interpret X as 1 because cannot represent them in cube. */
    return bdd_1 ();

  {
    BDDPTR T = BDD_COFACTOR_POS (f);
    BDDPTR E = BDD_COFACTOR_NEG (f);
    BDDPTR R;

    if (BDD_0_P (E)) {
      BDDPTR v = bdd_create_var (BDD_VARID (f));

      f = bdd_cube_factor (T);
      /* v belongs to cube factor: */
      R = bdd_and (v, f);
      bdd_free (v);
    }
    else
    if (BDD_0_P (T)) {
      BDDPTR v = bdd_create_var (BDD_VARID (f));

      f = bdd_cube_factor (E);
      /* v' belongs to cube factor: */
      R = bdd_less (v, f);
      bdd_free (v);
    }
    else {
      /* top var of f does not belong to cube factor: */
      f = bdd_or (T, E);
      R = bdd_cube_factor (f);
    }
    bdd_free (f);
    return R;
  }
}

#define swap_vars_interpret_mod_bits(f)	\
	bdd_invert_input_interpret_mod_bits (f)

static void bdd_swap_vars_aux (BDDPTR f)
{
  /* Mark wherever we go: */
  BDD_TOGGLE_MARK (f);

  if (BDD_TERM_P (f))
    BDD_AUX1_BDD (f) = PTR (bdd_assign (f));
  else {
    int   id = BDD_VARID (f);
    BDDPTR T = BDD_THEN  (f);
    BDDPTR E = BDD_ELSE  (f);
    BDDPTR v;

    if (BDD_NOT_MARKED (T, f)) bdd_swap_vars_aux (T);
    if (BDD_NOT_MARKED (E, f)) bdd_swap_vars_aux (E);
    T = swap_vars_interpret_mod_bits (T);
    E = swap_vars_interpret_mod_bits (E);

    v = bdd_create_var (id + (odd(id) ? -1 : 1));

    BDD_AUX1_BDD (f) = PTR (bdd_ite (v, T, E));
    bdd_free (v);
    bdd_free (T);
    bdd_free (E);
  }
}

/* Swaps the odd and even variable ids in f. */
BDDPTR bdd_swap_odd_even_vars (BDDPTR f)
{
  int save_bdd_do_dynamic_ordering;
  BDDPTR R;

  if (BDD_VOID_P (f) || BDD_TERM_P (f))
    return bdd_assign (f);

  save_bdd_do_dynamic_ordering = bdd_do_dynamic_ordering;
  bdd_do_dynamic_ordering = 0;
  bdd_swap_vars_aux (f);
  R = swap_vars_interpret_mod_bits (f);
  bdd_do_dynamic_ordering = save_bdd_do_dynamic_ordering;

  /* Also resets marks: */
  bdd_traverse_pre (f, bdd_free_aux1_action);

  return R;
}
