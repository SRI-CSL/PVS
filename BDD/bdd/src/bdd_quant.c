/*
 DOCUMENTATION INFORMATION				         module: BDD
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system	   : Apollo DN3000, HP 9000/S500, IBM RS/6000
 file	   : bdd_quant.c
 unit-title: 
 ref.	   :
 author(s) : Copyright (c) 1990-1998 G.L.J.M. Janssen
 date	   : 31-MAR-1998
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

/* Interpret the BDD's for E.f and A.f stored in the aux fields of f
   according the modification bits of f and the argument existential.

   BDD_AUX1_BDD (f) = E.(PTR (f))
   BDD_AUX2_BDD (f) = A.(PTR (f))

   1. !BDD_O_INV_EDGE (f) && !BDD_I_INV_EDGE (f)
      E.f = BDD_AUX1_BDD (f), A.f = BDD_AUX2_BDD (f).
       
   2. !BDD_O_INV_EDGE (f) &&  BDD_I_INV_EDGE (f)
      E.f = ~BDD_AUX1_BDD (f), A.f = ~BDD_AUX2_BDD (f).

   3.  BDD_O_INV_EDGE (f) && !BDD_I_INV_EDGE (f)
      E.f = BDD_AUX2_BDD (f)', A.f = BDD_AUX1_BDD (f)'.

   4.  BDD_O_INV_EDGE (f) &&  BDD_I_INV_EDGE (f)
      E.f = ~BDD_AUX2_BDD (f)', A.f = ~BDD_AUX1_BDD (f)'.

   Where ~ means the inversion of the top-variable but only when it is
   equal to the top-variable of f.
   pre: !BDD_VOID_P (f).
*/
static BDDPTR quantify_interpret_mod_bits (BDDPTR f, int existential)
{
  BDDPTR R;
  BDDPTR Ef = BDD_AUX1_BDD (f);
  BDDPTR Af = BDD_AUX2_BDD (f);

  if (existential)
    R = BDD_O_INV_EDGE_P (f) ? BDD_COMPL (Af) : Ef;
  else
    R = BDD_O_INV_EDGE_P (f) ? BDD_COMPL (Ef) : Af;

  if (BDD_I_INV_EDGE_P (f) && BDD_VARID (f) == BDD_VARID (R))
    return bdd_invert_input_top (R);
  return bdd_assign (R);
}

/* pre: !BDD_VOID_P (f)

   Uses aux1 field to store partial existential quantification result;
   Uses aux2 field to store partial universal   quantification result.
   Note: Ex.(f') = (Ax.f)'.

   Don't take modification bits of f into account, i.e.
   calculate result for PTR (f). Thus in the comments read PTR (f) for f.
   The final result is calculated by quantify_interpret_mod_bits.
*/
#if 0
static void bdd_quantify_c_aux (BDDPTR f, BDDPTR vars_cube)
{
  int topF = BDD_RANK (f);	/* f's top var rank number */
  int rank;

  /* Mark wherever we go: */
  BDD_TOGGLE_MARK (f);

 restart:

  /* E().f = f, E(x,y,z,...).Const = Const
     A().f = f, A(x,y,z,...).Const = Const
  */
  if (BDD_TERM_P (vars_cube) || BDD_TERM_P (f)) {
    /* Interpret as no quantification. */
    BDD_AUX1_BDD (f) = bdd_assign (PTR (f));
    BDD_AUX2_BDD (f) = bdd_assign (PTR (f));
    return;
  }

  /* BDD_INTERN_P (f) */

  rank = BDD_RANK (vars_cube);

  /* Bottom case: */
  if (rank < topF) {
    /* x !elem sup(f) ==> E(x,y,z,...).f = E(y,z,...).f
       x !elem sup(f) ==> A(x,y,z,...).f = A(y,z,...).f
    */
    /* Var not in support of f, consider the next one (higher in rank): */
    vars_cube = BDD_COFACTOR_POS (vars_cube);
    goto restart;
  }

  /* Here: topF <= rank < BDD_TERMID */
  {
    BDDPTR v, R1, R2;
    BDDPTR T = BDD_THEN (f);
    BDDPTR E = BDD_ELSE (f);

    if (rank == topF) {
      /*    f  =  x T  + x' E
	    f' = (x T  + x' E)'
	       = (x T)' (x' E)'
	       = (x' + T') (x + E')
	       = (x T' + x' E' + T' E')
	       = x T' + x' E'

	 E(x,y,z,...).f = Ex.(E(y,z,...).f)
	                = Ex.(E(y,z,...).(x T + x' E))
			= Ex.(E(y,z,...).(x T) + E(y,z,...).(x' E))
			= Ex.(x E(y,z,...).T + x' E(y,z,...).E)
			= E(y,z,...).T + E(y,z,...).E

	 A(x,y,z,...).f = (E(x,y,z,...).f')'
	                = (Ex.(E(y,z,...).(x T' + x' E')))'
			= (E(y,z,...).T' + E(y,z,...).E')'
			= A(y,z,...).T & A(y,z,...).E

	 Note: x disappears in result.
      */

      if (BDD_NOT_MARKED (T, f))
	bdd_quantify_c_aux (T, BDD_COFACTOR_POS (vars_cube));

      if (BDD_NOT_MARKED (E, f))
	bdd_quantify_c_aux (E, BDD_COFACTOR_POS (vars_cube));

      R1 = quantify_interpret_mod_bits (T, 1); /* E(y,z,...).T */
      R2 = quantify_interpret_mod_bits (E, 1); /* E(y,z,...).E */

      BDD_AUX1_BDD (f) = bdd_or  (R1, R2); /* E(x,y,z,...).f */
      bdd_free (R1);
      bdd_free (R2);

      R1 = quantify_interpret_mod_bits (T, 0); /* A(y,z,...).T */
      R2 = quantify_interpret_mod_bits (E, 0); /* A(y,z,...).E */

      BDD_AUX2_BDD (f) = bdd_and (R1, R2); /* A(x,y,z,...).f */
      bdd_free (R1);
      bdd_free (R2);
      return;
    }

    /* Here: rank > topF. */

    /* f  =  v T  + v' E
       f' = (v T  + v' E)'
	  = (v T)' (v' E)'
	  = (v' + T') (v + E')
	  = (v T' + v' E' + T' E')
	  = v T' + v' E'

       rank(v) < rank(x) < rank(y) < rank(z) ...

       E(x,y,z,...).f = E(x,y,z,...).(v T + v' E)
		      = E(x,y,z,...).(v T) + E(x,y,z,...).(v' E)
		      = v E(x,y,z,...).T + v' E(x,y,z,...).E
		      = ite (v, E(x,y,z,...).T, E(x,y,z,...).E)

       A(x,y,z,...).f = (E(x,y,z,...).f')'
                      = (E(x,y,z,...).(v T' + v' E'))'
		      = (E(x,y,z,...).(v T') + E(x,y,z,...).(v' E'))'
		      = (v E(x,y,z,...).T' + v' E(x,y,z,...).E')'
		      = (ite (v, E(x,y,z,...).T', E(x,y,z,...).E'))'
		      = (ite (v, (A(x,y,z,...).T)', (A(x,y,z,...).E)'))'
		      = ite (v, A(x,y,z,...).T, A(x,y,z,...).E)

       Note: x might disappear in result.
    */

    /* Recursive case 2: */
    if (BDD_NOT_MARKED (T, f))
      /* Not yet been there, so recurse: */
      bdd_quantify_c_aux (T, vars_cube);

    if (BDD_NOT_MARKED (E, f))
      /* Not yet been there, so recurse: */
      bdd_quantify_c_aux (E, vars_cube);

    v = bdd_create_var (BDD_VARID (f));

    R1 = quantify_interpret_mod_bits (T, 1); /* E(x,y,z,...).T */
    R2 = quantify_interpret_mod_bits (E, 1); /* E(x,y,z,...).E */

    BDD_AUX1_BDD (f) = bdd_ite (v, R1, R2); /* E(x,y,z,...).f */
    bdd_free (R1);
    bdd_free (R2);

    R1 = quantify_interpret_mod_bits (T, 0); /* A(x,y,z,...).T */
    R2 = quantify_interpret_mod_bits (E, 0); /* A(x,y,z,...).E */

    BDD_AUX2_BDD (f) = bdd_ite (v, R1, R2); /* A(x,y,z,...).f */
    bdd_free (R1);
    bdd_free (R2);
    bdd_free (v);
  }
}
#endif

/* Faster version suggested by Koen van Eijk.
   Calculates partial results on demand, thus avoiding the calculation
   of both existential and universal quantification for all nodes.
   The result for existential quantification will normally go in AUX1;
   the result for universal quantification will normally go in AUX2.
   The aux field for the dual quantor will be set to BDD_VOID.
   The alternative would be to calculate both universal and existential
   quantification for all BDD nodes, even if only one of them is requested
   by the top-level routine.
   It is hoped that our demand-driven approach saves time.

   The argument existential can in fact have 3 values:
    0	:	existential quantification,
    1	:	universal   quantification,
   >1	:	existential and universal quantification for top node only!

   Pre: !BDD_TERM_P (vars_cube), but perhaps BDD_TERM_P (f).
*/
static BDDPTR bdd_quantify_c_aux (int existential, BDDPTR f, BDDPTR vars_cube)
{
  int topF = BDD_RANK (f);
  int rank = BDD_RANK (vars_cube);
  int neg_result = 0;
  int inv_result = 0;

  /* Mark wherever we go: */
  if (!BDD_MARK (f)) {
    BDD_TOGGLE_MARK (f);
    /* For safety clear the aux fields first time we visit a node: */
    BDD_AUX1_BDD (f) = BDD_VOID;
    BDD_AUX2_BDD (f) = BDD_VOID;
  }

  while (rank < topF) {
    /* Var not in support of f, consider the next one (higher in rank): */
    vars_cube = BDD_COFACTOR_POS (vars_cube);
    rank = BDD_RANK (vars_cube);
  }

  if (BDD_TERM_P (f) || BDD_TERM_P (vars_cube)) {
    /* Constant f: no variables to quantify. */
    /* Constant (BDD_1) vars_cube: interpret as no quantification. */
    BDD_AUX1_BDD (f) = bdd_assign (PTR (f));
    BDD_AUX2_BDD (f) = bdd_assign (PTR (f));
    return bdd_assign (f);
  }

  /* Here: topF <= rank < BDD_TERMID */

  if (BDD_O_INV_EDGE_P (f)) {
    if (existential < 2) {
      existential = !existential;
      neg_result = 1;
    }
    /* else: do both and not interested in return value for top node. */
  }

  if (BDD_I_INV_EDGE_P (f))
    inv_result = rank != topF;

  f = PTR (f);

  {
    BDDPTR T = BDD_THEN (f);
    BDDPTR E = BDD_ELSE (f);
    BDDPTR R = BDD_VOID;

    if (existential >= 1) {	/* just existential or both */
      if (BDD_VOID_P (BDD_AUX1_BDD (f))) {
	if (topF == rank) {
	  /* Use some short cuts: */
	  if (BDD_1_P (T) || BDD_1_P (E))
	    BDD_AUX1_BDD (f) = bdd_1 ();
	  else {
	    T = bdd_quantify_c_aux (1, T, vars_cube);
	    if (BDD_1_P (T))
	      BDD_AUX1_BDD (f) = T;
	    else {
	      E = bdd_quantify_c_aux (1, E, vars_cube);
	      BDD_AUX1_BDD (f) = bdd_or (T, E);
	      bdd_free (T);
	      bdd_free (E);
	    }
	  }
	}
	else { /* topF < rank */
	  BDDPTR v = bdd_create_var (BDD_VARID (f)); 

	  T = bdd_quantify_c_aux (1, T, vars_cube);
	  E = bdd_quantify_c_aux (1, E, vars_cube);
	  BDD_AUX1_BDD (f) = bdd_ite (v, T, E);
	  bdd_free (v);
	  bdd_free (T);
	  bdd_free (E);
	}
      }
      R = BDD_AUX1_BDD (f);
    }

    /* Might have modified these values: */
    T = BDD_THEN (f);
    E = BDD_ELSE (f);

    if (!existential || existential > 1) { /* just universal or both */
      if (BDD_VOID_P (BDD_AUX2_BDD (f))) {
	if (topF == rank) {
	  /* Use some short cuts: */
	  if (BDD_0_P (T) || BDD_0_P (E))
	    BDD_AUX2_BDD (f) = bdd_0 ();
	  else {
	    T = bdd_quantify_c_aux (0, T, vars_cube);
	    if (BDD_0_P (T))
	      BDD_AUX2_BDD (f) = T;
	    else {
	      E = bdd_quantify_c_aux (0, E, vars_cube);
	      BDD_AUX2_BDD (f) = bdd_and (T, E);
	      bdd_free (T);
	      bdd_free (E);
	    }
	  }
	}
	else { /* topF < rank */
	  BDDPTR v = bdd_create_var (BDD_VARID (f)); 

	  T = bdd_quantify_c_aux (0, T, vars_cube);
	  E = bdd_quantify_c_aux (0, E, vars_cube);
	  BDD_AUX2_BDD (f) = bdd_ite (v, T, E);
	  bdd_free (v);
	  bdd_free (T);
	  bdd_free (E);
	}
      }
      R = BDD_AUX2_BDD (f);
    }

    if (existential > 1)
      return BDD_VOID;

    if (neg_result)
      R = BDD_COMPL (R);

    return inv_result ? bdd_invert_input_top (R) : bdd_assign (R);
  }
}

/* Calculate Q(vars).f, i.e. quantify f either existentially or universally
   over all variables that appear in the vars_cube.

   Uses the following identities:

   E(x1,x2,x3,...).f = E x1 (E(x2,x3,...).f)
   A(x1,x2,x3,...).f = A x1 (A(x2,x3,...).f)

   f = g'
   E.f = E.g' = (A.g)'
   A.f = A.g' = (E.g)'

   Pre: vars_cube must be a BDD cube of positive literals only.
*/
BDDPTR bdd_quantify_c (int existential, BDDPTR f, BDDPTR vars_cube)
{
  int save_bdd_do_dynamic_ordering;
  BDDPTR R;

  if (BDD_VOID_P (f) || BDD_VOID_P (vars_cube))
    return BDD_VOID;

  if (BDD_1_P (vars_cube) || BDD_TERM_P (f))
    /* Empty list: interpret as no quantification. */
    /* Constant f: f does not depend on any variable. */
    return bdd_assign (f);

  if (BDD_TERM_P (vars_cube))
    /* Interpret as quantification over all variables.
       Here f is non-constant.
    */
    return existential ? bdd_1 () : bdd_0 ();

  save_bdd_do_dynamic_ordering = bdd_do_dynamic_ordering;
  bdd_do_dynamic_ordering = 0;

  R = bdd_quantify_c_aux (existential, f, vars_cube);
  /*  R = quantify_interpret_mod_bits (f, existential);*/

  bdd_do_dynamic_ordering = save_bdd_do_dynamic_ordering;

  /* Also resets marks: */
  bdd_traverse_pre (f, bdd_free_aux1_and_aux2_action);

  return R;
}

/* pre: !BDD_VOID_P (f) && all vars in list have rank < BDD_TERMID */
static void bdd_quantify_aux (BDDPTR f, BDD_ELEM vars)
{
  int topF = BDD_RANK (f);	/* f's top var rank number */
  int x;			/* the list element's variable */
  int rank;			/* its rank number */

  /* Mark wherever we go: */
  BDD_TOGGLE_MARK (f);

 restart:

  /* E().f = f, E(x,y,z,...).Const = Const
     A().f = f, A(x,y,z,...).Const = Const
  */
  if (!vars || BDD_TERM_P (f)) {
    /* Interpret as no quantification. */
    BDD_AUX1_BDD (f) = bdd_assign (PTR (f));
    BDD_AUX2_BDD (f) = bdd_assign (PTR (f));
    return;
  }

  /* BDD_INTERN_P (f) */

  /* id's might be negative to indicate negative literals.
     For quantification consider them positive:
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
    /* x !elem sup(f) ==> E(x,y,z,...).f = E(y,z,...).f
       x !elem sup(f) ==> A(x,y,z,...).f = A(y,z,...).f
    */
    /* Var not in support of f, consider the next one (higher in rank): */
    vars = BDD_LIST_NEXT (vars);
    goto restart;
  }

  /* Here: topF <= rank < BDD_TERMID */
  {
    BDDPTR v, R1, R2;
    BDDPTR T = BDD_THEN (f);
    BDDPTR E = BDD_ELSE (f);

    if (rank == topF) {
      /*    f  =  x T  + x' E
	    f' = (x T  + x' E)'
	       = (x T)' (x' E)'
	       = (x' + T') (x + E')
	       = (x T' + x' E' + T' E')
	       = x T' + x' E'

	 E(x,y,z,...).f = Ex.(E(y,z,...).f)
	                = Ex.(E(y,z,...).(x T + x' E))
			= Ex.(E(y,z,...).(x T) + E(y,z,...).(x' E))
			= Ex.(x E(y,z,...).T + x' E(y,z,...).E)
			= E(y,z,...).T + E(y,z,...).E

	 A(x,y,z,...).f = (E(x,y,z,...).f')'
	                = (Ex.(E(y,z,...).(x T' + x' E')))'
			= (E(y,z,...).T' + E(y,z,...).E')'
			= A(y,z,...).T & A(y,z,...).E

	 Note: x disappears in result.
      */

      if (BDD_NOT_MARKED (T, f))
	bdd_quantify_aux (T, BDD_LIST_NEXT (vars));

      if (BDD_NOT_MARKED (E, f))
	bdd_quantify_aux (E, BDD_LIST_NEXT (vars));

      R1 = quantify_interpret_mod_bits (T, 1); /* E(y,z,...).T */
      R2 = quantify_interpret_mod_bits (E, 1); /* E(y,z,...).E */

      BDD_AUX1_BDD (f) = bdd_or  (R1, R2); /* E(x,y,z,...).f */
      bdd_free (R1);
      bdd_free (R2);

      R1 = quantify_interpret_mod_bits (T, 0); /* A(y,z,...).T */
      R2 = quantify_interpret_mod_bits (E, 0); /* A(y,z,...).E */

      BDD_AUX2_BDD (f) = bdd_and (R1, R2); /* A(x,y,z,...).f */
      bdd_free (R1);
      bdd_free (R2);
      return;
    }

    /* Here: rank > topF. */

    /* f  =  v T  + v' E
       f' = (v T  + v' E)'
	  = (v T)' (v' E)'
	  = (v' + T') (v + E')
	  = (v T' + v' E' + T' E')
	  = v T' + v' E'

       rank(v) < rank(x) < rank(y) < rank(z) ...

       E(x,y,z,...).f = E(x,y,z,...).(v T + v' E)
		      = E(x,y,z,...).(v T) + E(x,y,z,...).(v' E)
		      = v E(x,y,z,...).T + v' E(x,y,z,...).E
		      = ite (v, E(x,y,z,...).T, E(x,y,z,...).E)

       A(x,y,z,...).f = (E(x,y,z,...).f')'
                      = (E(x,y,z,...).(v T' + v' E'))'
		      = (E(x,y,z,...).(v T') + E(x,y,z,...).(v' E'))'
		      = (v E(x,y,z,...).T' + v' E(x,y,z,...).E')'
		      = (ite (v, E(x,y,z,...).T', E(x,y,z,...).E'))'
		      = (ite (v, (A(x,y,z,...).T)', (A(x,y,z,...).E)'))'
		      = ite (v, A(x,y,z,...).T, A(x,y,z,...).E)

       Note: x might disappear in result.
    */

    /* Recursive case 2: */
    if (BDD_NOT_MARKED (T, f))
      /* Not yet been there, so recurse: */
      bdd_quantify_aux (T, vars);

    if (BDD_NOT_MARKED (E, f))
      /* Not yet been there, so recurse: */
      bdd_quantify_aux (E, vars);

    v = bdd_create_var (BDD_VARID (f));

    R1 = quantify_interpret_mod_bits (T, 1); /* E(x,y,z,...).T */
    R2 = quantify_interpret_mod_bits (E, 1); /* E(x,y,z,...).E */

    BDD_AUX1_BDD (f) = bdd_ite (v, R1, R2); /* E(x,y,z,...).f */
    bdd_free (R1);
    bdd_free (R2);

    R1 = quantify_interpret_mod_bits (T, 0); /* A(x,y,z,...).T */
    R2 = quantify_interpret_mod_bits (E, 0); /* A(x,y,z,...).E */

    BDD_AUX2_BDD (f) = bdd_ite (v, R1, R2); /* A(x,y,z,...).f */
    bdd_free (R1);
    bdd_free (R2);
    bdd_free (v);
  }
}

BDDPTR bdd_quantify (int existential, BDDPTR f, BDD_LIST vars)
{
  int save_bdd_do_dynamic_ordering;
  BDDPTR R;

  if (BDD_VOID_P (f))
    return BDD_VOID;

  if (!vars || BDD_TERM_P (f))
    /* Empty list: interpret as no quantification. */
    /* Constant f: f does not depend on any variable. */
    return bdd_assign (f);

  save_bdd_do_dynamic_ordering = bdd_do_dynamic_ordering;
  bdd_do_dynamic_ordering = 0;

  bdd_quantify_aux (f, BDD_LIST_FIRST (vars));
  R = quantify_interpret_mod_bits (f, existential);

  bdd_do_dynamic_ordering = save_bdd_do_dynamic_ordering;

  /* Also resets marks: */
  bdd_traverse_pre (f, bdd_free_aux1_and_aux2_action);

  return R;
}

/* Modifies f_vec by calling bdd_quantify for each element.
   Original BDD elements are bdd_freed.
   Returns modified f_vec.
*/
BDDPTR *bdd_quantify_vec (int existential, BDDPTR *f_vec, int size,
			  BDD_LIST vars)
{
  int i;
  int save_bdd_do_dynamic_ordering;

  if (!vars)
    return f_vec;

  save_bdd_do_dynamic_ordering = bdd_do_dynamic_ordering;
  bdd_do_dynamic_ordering = 0;

  /* Assume all mark fields 0. */
  for (i = 0; i < size; i++) {
    BDDPTR f = f_vec[i];

    if (!BDD_VOID_P (f)) {

      if (BDD_MARK (f)) {
	BDDPTR R = quantify_interpret_mod_bits (f, existential);

	bdd_free (f);
	f_vec[i] = R;
      }
      else
	bdd_quantify_aux (f, BDD_LIST_FIRST (vars));
    }
  } /*for*/
  /* Now all mark fields are set. */

  for (i = 0; i < size; i++) {
    BDDPTR f = f_vec[i];

    if (!BDD_VOID_P (f) && BDD_MARK (f)) {
      BDDPTR R = quantify_interpret_mod_bits (f, existential);

      /* Also resets marks: */
      bdd_traverse_pre (f, bdd_free_aux1_and_aux2_action);

      bdd_free (f);
      f_vec[i] = R;
    }
  }
  bdd_do_dynamic_ordering = save_bdd_do_dynamic_ordering;

  return f_vec;
}

/* Modifies f_vec by calling bdd_quantify_c for each element.
   Original BDD elements are bdd_freed.
   Returns modified f_vec or NULL when BDD_VOID_P (vars_cube).
   If BDD_1_P (vars_cube) no quantification takes place;
   if it is a non-BDD_1 constant, quantification over all variables
   takes place.
*/
BDDPTR *bdd_quantify_c_vec (int existential, BDDPTR *f_vec, int size,
			    BDDPTR vars_cube)
{
  BDDPTR *save_vec;
  int i;
  int save_bdd_do_dynamic_ordering;

  if (!size || !f_vec || BDD_VOID_P (vars_cube))
    return NULL;

  if (BDD_1_P (vars_cube))
    /* Empty list: interpret as no quantification. */
    return f_vec;

  if (BDD_TERM_P (vars_cube)) {
    /* Interpret as quantification over all variables. */
    for (i = 0; i < size; i++) {
      BDDPTR f = f_vec[i];

      if (!BDD_VOID_P (f) && !BDD_TERM_P (f)) {
	/* Interpret as quantification over all variables.
	   Here f is non-constant.
	*/
	bdd_free (f);
	f_vec[i] = existential ? bdd_1 () : bdd_0 ();
      }
    }
    return f_vec;
  }

  save_bdd_do_dynamic_ordering = bdd_do_dynamic_ordering;
  bdd_do_dynamic_ordering = 0;

  /* Assume all mark fields 0. */
  for (i = 0; i < size; i++) {
    BDDPTR f = f_vec[i];

    if (!BDD_VOID_P (f))
      /* !BDD_TERM_P (vars_cube) */
      bdd_quantify_c_aux (2 /*both*/, f, vars_cube);
  }
  /* Now all mark fields are set. */

  save_vec = MALLOC_ARRAY (size, BDDPTR);

  for (i = 0; i < size; i++) {
    BDDPTR f = f_vec[i];

    save_vec[i] = f;

    if (!BDD_VOID_P (f))
      f_vec[i] = quantify_interpret_mod_bits (f, existential);
  }

  for (i = 0; i < size; i++) {
    BDDPTR f = save_vec[i];

    if (!BDD_VOID_P(f) && BDD_MARK (f))
      /* Also resets marks: */
      bdd_traverse_pre (f, bdd_free_aux1_and_aux2_action);

    bdd_free (f);
  }
  MA_FREE_ARRAY (save_vec, size, BDDPTR);

  bdd_do_dynamic_ordering = save_bdd_do_dynamic_ordering;

  return f_vec;
}

static BDDPTR bdd_quant_1 (int existential, BDDPTR f, int x)
{
  BDDPTR v = bdd_create_var (abs (x));
  BDDPTR R = bdd_quantify_c (existential, f, v);

  bdd_free (v);
  return R;
}

BDDPTR bdd_smooth (BDDPTR f, int x)
{
  return bdd_quant_1 (1, f, x);
}

BDDPTR bdd_consensus (BDDPTR f, int x)
{
  return bdd_quant_1 (0, f, x);
}

/* With 2^13 = 8192, spending ~100kbytes on cache. */
#define BDD_AND_SMOOTH_CACHE_LOG2SIZE	13

typedef struct bdd_and_smooth_cache_entry BDD_AND_SMOOTH_CACHE_ENTRY;

/* Per entry: 3 * sizeof (BDDPTR) = 12 bytes. */
static struct bdd_and_smooth_cache_entry {
  BDDPTR f;
  BDDPTR g;
  BDDPTR R;
} bdd_and_smooth_cache[pow2(BDD_AND_SMOOTH_CACHE_LOG2SIZE)];

/* Using muliplicative method in hashing. Constant A according Knuth:
   A = (PHI - 1) * 2^32, with PHI = golden ratio = (1 + sqrt(5))/2
   PHI-1 = 0.61803 39887 49894 84820 ...
   A = 26544357690 = -1640531527 = 0x9E3779B9U
*/

/* Hashes Nat `k' to a value in the range [0..2^log2size-1]. */
#define BDD_HASH(k,log2size)	div_pow2(0x9E3779B9U*(k), NAT_BIT-(log2size))

#define hash_and_smooth(a, b) \
    BDD_HASH((((Nat) (a)) ^ ((Nat) (b) << 15)), BDD_AND_SMOOTH_CACHE_LOG2SIZE)

static BDDPTR bdd_lookup_and_smooth_cache (BDDPTR f, BDDPTR g)
{
  BDDPTR R;
  BDD_AND_SMOOTH_CACHE_ENTRY *entry;

  /* Make canonical: `and' operation is commutative. */
  if (f > g) {
    R = f;
    f = g;
    g = R;
  }

  entry = bdd_and_smooth_cache + hash_and_smooth (f, g);

  if (   (R = entry->R)
      && BDD_EQUAL_P (f, entry->f)
      && BDD_EQUAL_P (g, entry->g))
    return bdd_assign (R);
  return BDD_VOID;
}

static BDDPTR bdd_insert_and_smooth_cache (BDDPTR f, BDDPTR g, BDDPTR R)
{
  BDD_AND_SMOOTH_CACHE_ENTRY *entry;

  /* Make canonical: `and' operation is commutative. */
  if (f > g) {
    BDDPTR t = f;
    f = g;
    g = t;
  }

  entry = bdd_and_smooth_cache + hash_and_smooth (f, g);

  /* Let's be honest, check for equal data: */
#ifdef COMMENT
  /* Who cares? */
  if (   entry->R
      && BDD_EQUAL_P (f, entry->f)
      && BDD_EQUAL_P (g, entry->g))
    /* Already stored. */
    return R;
#endif

  bdd_assign (f);
  bdd_assign (g);
  bdd_assign (R);
  if (entry->R) {
    bdd_free (entry->f);
    bdd_free (entry->g);
    bdd_free (entry->R);
  }
  /* else a fresh entry: */

  entry->f = f;
  entry->g = g;
  return entry->R = R;
}

static void bdd_cleanup_and_smooth_cache (void)
{
  register int size = pow2(BDD_AND_SMOOTH_CACHE_LOG2SIZE);
  register BDD_AND_SMOOTH_CACHE_ENTRY *entry = bdd_and_smooth_cache;

  while (size--) {
    if (entry->R) {
      bdd_free (entry->f);
      bdd_free (entry->g);
      bdd_free (entry->R);
      entry->R = BDD_VOID;
    }
    entry++;
  }
}

/* Uses its private bdd_and_smooth_cache for intermediate results. */
static BDDPTR bdd_and_smooth_aux (BDDPTR f, BDDPTR g, BDD_ELEM vars)
{
  int topF = BDD_RANK (f);	/* f's top var rank number */
  int topG = BDD_RANK (g);	/* g's top var rank number */
  int minr = topF < topG ? topF : topG;	/* the smallest of the 2 */
  int x;			/* the list element's variable */
  int rank;			/* its rank number */
  BDDPTR R;

  /* Get id in list such that rank(id) >= minr.
     id's might be negative to indicate negative literals.
     For quantification consider them positive:
  */
  for (; vars; vars = BDD_LIST_NEXT (vars)) {
    if ((x = BDD_ELEM_CONTENTS_I (vars)) < 0)
      x = -x;
    rank = BDD_VAR_RANK (x);

    if (rank >= minr)
      /* Var probably in support of f and g. */
      break;
  } /*for*/

  if (!vars)
    /* Empty list: interpret as no quantification. */
    return bdd_and (f, g);

  if (BDD_TERM_P (f) || BDD_TERM_P (g)) {
    BDDPTR t;

    /* Either f or g or both constant: */
    t = bdd_and (f, g);
    bdd_quantify_aux (t, vars);
    R = quantify_interpret_mod_bits (t, 1 /*=existential*/);
    /* Also resets marks: */
    bdd_traverse_pre (t, bdd_free_aux1_and_aux2_action);
    bdd_free (t);

    return R;
  }

  if (!BDD_VOID_P (R = bdd_lookup_and_smooth_cache (f, g)))
    return R;

  { /* Here: topF,topG <= rank < BDD_TERMID */
    BDDPTR v, T, E;
    BDDPTR T_f = topF == minr ? BDD_COFACTOR_POS (f) : f;
    BDDPTR E_f = topF == minr ? BDD_COFACTOR_NEG (f) : f;
    BDDPTR T_g = topG == minr ? BDD_COFACTOR_POS (g) : g;
    BDDPTR E_g = topG == minr ? BDD_COFACTOR_NEG (g) : g;

    /* E x . (f & g) = E x . (y T_f + y' E_f) & (y T_g + y' E_g) */
    /*               = E x . (y T_f T_g + y' E_f E_g) */
    T = bdd_and_smooth_aux (T_f, T_g, vars);
    if (rank == minr) {
      /* E x . (f & g) = T_f T_g + E_f E_g */
      if (BDD_1_P (T))
	R = bdd_1 ();
      else {
	E = bdd_and_smooth_aux (E_f, E_g, vars);
	R = bdd_or (T, E);
	bdd_free (E);
      }
    }
    else {
      /* E x . (f & g) = y E x . T_f T_g + y' E x . E_f E_g */
      E = bdd_and_smooth_aux (E_f, E_g, vars);
      v = bdd_create_var (topF < topG ? BDD_VARID (f) : BDD_VARID (g));
      R = bdd_ite (v, T, E);
      bdd_free (v);
      bdd_free (E);
    }
    bdd_free (T);
  }
  return bdd_insert_and_smooth_cache (f, g, R);
}

/* Calculates E(vars).(f & g),
   i.e. quantify the product of f and g existentially
   over all variables that appear in the list vars. This list contains
   the id's of the variables in an increasing order according their
   rank numbers.
   (It is allowed that a variable occurs more than once
   in the list; of course then they must be consecutive occurrences).

   [ Clarke et.al. call this the RelProd (= Relational Product) algorithm.]
*/
BDDPTR bdd_and_smooth (BDDPTR f, BDDPTR g, BDD_LIST vars)
{
  int save_bdd_do_dynamic_ordering;
  BDDPTR R;

  if (BDD_VOID_P (f) || BDD_VOID_P (g))
    return BDD_VOID;

  if (!vars)
    /* Empty list: interpret as no quantification. */
    return bdd_and (f, g);

  save_bdd_do_dynamic_ordering = bdd_do_dynamic_ordering;
  bdd_do_dynamic_ordering = 0;

  R = bdd_and_smooth_aux (f, g, BDD_LIST_FIRST (vars));

  bdd_do_dynamic_ordering = save_bdd_do_dynamic_ordering;

  bdd_cleanup_and_smooth_cache ();

  return R;
}

/*
   Pre: !BDD_TERM_P (vars_cube), but perhaps BDD_TERM_P (f/g).
*/
static BDDPTR bdd_and_smooth_c_aux (BDDPTR f, BDDPTR g, BDDPTR vars_cube)
{
  int topF;	/* f's top var rank number */
  int topG;	/* g's top var rank number */
  int minr;	/* the smallest of the 2 */
  int rank;
  BDDPTR R;

  if (BDD_TERM_P (f) || BDD_TERM_P (g)) {
    BDDPTR t;

    /* Either f or g or both constant: */
    t = bdd_and (f, g);
    /* Must have !BDD_TERM_P (vars_cube) */
    R = bdd_quantify_c_aux (1 /*=existential*/, t, vars_cube);
    /* Also resets marks: */
    bdd_traverse_pre (t, bdd_free_aux1_and_aux2_action);
    bdd_free (t);

    return R;
  }

  /* Here neither f nor g constant. */
  topF = BDD_RANK (f);
  topG = BDD_RANK (g);
  minr = topF < topG ? topF : topG;
  rank = BDD_RANK (vars_cube);

  while (rank < minr) {
    /* Var not in support of f/g, consider the next one (higher in rank): */
    vars_cube = BDD_COFACTOR_POS (vars_cube);
    rank = BDD_RANK (vars_cube);
  }

  if (BDD_TERM_P (vars_cube))
    /* Vars cube exhausted: interpret as no quantification. */
    return bdd_and (f, g);

  /* Here: min(topF,topG) <= rank < BDD_TERMID */

  if (!BDD_VOID_P (R = bdd_lookup_and_smooth_cache (f, g)))
    return R;

  {
    BDDPTR v, T, E;
    BDDPTR T_f = topF == minr ? BDD_COFACTOR_POS (f) : f;
    BDDPTR E_f = topF == minr ? BDD_COFACTOR_NEG (f) : f;
    BDDPTR T_g = topG == minr ? BDD_COFACTOR_POS (g) : g;
    BDDPTR E_g = topG == minr ? BDD_COFACTOR_NEG (g) : g;

    /* E x . (f & g) = E x . (y T_f + y' E_f) & (y T_g + y' E_g) */
    /*               = E x . (y T_f T_g + y' E_f E_g) */
    T = bdd_and_smooth_c_aux (T_f, T_g, vars_cube);
    if (rank == minr) {
      /* E x . (f & g) = T_f T_g + E_f E_g */
      if (BDD_1_P (T))
	R = bdd_1 ();
      else {
	E = bdd_and_smooth_c_aux (E_f, E_g, vars_cube);
	R = bdd_or (T, E);
	bdd_free (E);
      }
    }
    else {
      /* E x . (f & g) = y E x . T_f T_g + y' E x . E_f E_g */
      E = bdd_and_smooth_c_aux (E_f, E_g, vars_cube);
      v = bdd_create_var (topF < topG ? BDD_VARID (f) : BDD_VARID (g));
      R = bdd_ite (v, T, E);
      bdd_free (v);
      bdd_free (E);
    }
    bdd_free (T);
  }
  return bdd_insert_and_smooth_cache (f, g, R);
}

BDDPTR bdd_and_smooth_c (BDDPTR f, BDDPTR g, BDDPTR vars_cube)
{
  int save_bdd_do_dynamic_ordering;
  BDDPTR R;

  if (BDD_VOID_P (f) || BDD_VOID_P (g) || BDD_VOID_P (vars_cube))
    return BDD_VOID;

  if (BDD_1_P (vars_cube))
    /* The BDD_1 cube: interpret as no quantification. */
    return bdd_and (f, g);

  if (BDD_TERM_P (vars_cube))
    /* Interpret as quantification over all variables. */
    return BDD_0_P (bdd_ite_const (f, g, BDD_0)) ? bdd_0 () : bdd_1 ();

  save_bdd_do_dynamic_ordering = bdd_do_dynamic_ordering;
  bdd_do_dynamic_ordering = 0;

#if 0
  if (   ehv_bdd_last_smooth_set
      && !BDD_EQUAL_P (ehv_bdd_last_smooth_set, vars_cube)) {
    ehv_bdd_cleanup_and_smooth_cache ();
    ehv_bdd_last_smooth_set = ehv_bdd_assign (vars_cube);
  }
#endif

  R = bdd_and_smooth_c_aux (f, g, vars_cube);

  bdd_cleanup_and_smooth_cache ();

  bdd_do_dynamic_ordering = save_bdd_do_dynamic_ordering;

  return R;
}
