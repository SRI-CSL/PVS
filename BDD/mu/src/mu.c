/*
 DOCUMENTATION INFORMATION				   module: MU CALCULUS
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system    : Hewlett-Packard HP9000/S750
 file	   : mu.c
 unit-title: 
 ref.	   : 
 author(s) : Copyright (c) 1992-1997 G.L.J.M. Janssen
 date	   :  6-FEB-1997
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

/* ------------------------------------------------------------------------ */
/* INCLUDES                                                                 */
/* ------------------------------------------------------------------------ */

#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#include "alloc.h"
#include "hash.h"
#include "bdd_fns.h"
#include "appl.h"
#include "bdd_vfns.h"

#include "mu.h"

/* ------------------------------------------------------------------------ */
/* IMPORTED VARIABLES                                                       */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* LOCAL DEFINES                                                            */
/* ------------------------------------------------------------------------ */

#define CLOCKS_PER_MSEC	(CLOCKS_PER_SEC/1000.0)

/* Use as: "forever { ... }" or "do { ... } forever" */
#define forever		while(1)

/* Use as: "do { ... } once" */
#define once		while(0)

/* Define USE_CACHE to enable the caching of the BDD results of intermediate 
   formulas and terms in so far they do not depend on relational variables
   that are bound outside of them (this is automatically checked).
   Using the cache should generally speed up the calculation of fixed-point
   terms since these cause repeated interpretations.
   (Usually only switched off for debugging purposes!)
   When using mu predominantly for fixed-point and reachability calculations,
   it is observed that the effect of the caching is very small and sometimes
   even incurs an additional run-time overhead. I like to leave it off!
*/
/*#define USE_CACHE*/

/* Define DO_CHECKS to include code that does run-time checks.
   For now, only monotonicity of functions during fixed-point calculations is
   checked.
*/
#define DO_CHECKS

#define ODD(n)			((n)  & 1)
#define MUL2(n)			((n) << 1)
#define DIV2(n)			((n) >> 1)

/* User introduced variables have id's: 1,2,3,...
   Their name strings are stored in the signature hash table at
   corresponding indices.
   Their BDD indices are the even numbers: 0,2,4,...

   Occasionally the mu program itself will need to create some fresh
   user variables. To avoid name clashes with any existing
   user-introduced variables, these variables will be named
   "#n" with n = 0, 1, 2, etc.
*/

/* Get the BDD index for a user introduced variable with id `i': */
#define VAR_ID_2_BDD_IDX(i)	MUL2(i-1)
/* Get the user variable id for the BDD index `b': */
#define BDD_IDX_2_VAR_ID(b)	(DIV2(b)+1)
/* Place holders variables (PHVs) have id's: 0,1,2,...
   Their name strings are generated as "$<id>". These are NOT explicitly
   stored anywhere. Their BDD indices are odd: 1,3,5,...
*/

/* Get the BDD index for place holder variable `i': */
#define PHV_ID_2_BDD_IDX(i)	(MUL2(i) | 1)  /* = 2*i+1 */
/* Get the place holder variable for BDD index `b': */
#define BDD_IDX_2_PHV_ID(b)	DIV2(b)        /* = (b-1)/2, b odd */

/* Access macros to Formula fields: */
#define F_TYPE(F)		((F)->type)
#define F_VAR(F)		((F)->kind.var)
#define F_SUB(F)		((F)->kind.unary.sub)
#define F_LEFT(F)		((F)->kind.binary.left)
#define F_RIGHT(F)		((F)->kind.binary.right)
#define F_COND(F)		((F)->kind.ite.cond)
#define F_THEN(F)		((F)->kind.ite.then_part)
#define F_ELSE(F)		((F)->kind.ite.else_part)
#define F_QUANT_VARS(F)		((F)->kind.quantification.vars)
#define F_QUANT_F(F)		((F)->kind.quantification.sub)
#define F_APPLY_TERM(F)		((F)->kind.application.term)
#define F_APPLY_SUBS(F)		((F)->kind.application.subs)
#define F_ONE_OF_SUBS(F)	((F)->kind.one_of.subs)
#define F_NONE_OF_SUBS(F)	((F)->kind.none_of.subs)
#define F_SUBST_F(F)		((F)->kind.subst.f)
#define F_SUBST_VAR(F)		((F)->kind.subst.var)
#define F_SUBST_G(F)		((F)->kind.subst.g)
#define F_BDD(F)		((F)->val)
#define F_NEXT(F)		((F)->next)

/* Access macros to Term fields: */
#define T_TYPE(T)		((T)->type)
#define T_ARITY(T)		((T)->arity)
#define T_R_VAR(T)		((T)->kind.R_var.var)
#define T_R_BINDINGS(T)		((T)->kind.R_var.bindings)
#define T_R_TERM(T)		((T)->kind.R_var.def)
#define T_SUB(T)		((T)->kind.unary.sub)
#define T_LEFT(T)		((T)->kind.binary.left)
#define T_RIGHT(T)		((T)->kind.binary.right)
#define T_LAMBDA_VARS(T)	((T)->kind.abstraction.vars)
#define T_LAMBDA_F(T)		((T)->kind.abstraction.f)
#define T_FP_VAR(T)		((T)->kind.fixed_point.var)
#define T_FP_TERM(T)		((T)->kind.fixed_point.term)
#define T_FP_ENV(T)		((T)->kind.fixed_point.other_bound_vars)
#define T_FP_ITER_BOUND(T)	((T)->kind.fixed_point.iter_bound)
#define T_REACH_N(T)		((T)->kind.reach.N)
#define T_REACH_S0(T)		((T)->kind.reach.S0)
#define T_REACH_INV(T)		((T)->kind.reach.Inv)
#define T_BDD(T)		((T)->val)
#define T_NEXT(T)		((T)->next)

/* Special arity field values for relational term:
   It is an invariant that
   MU_ANY_ARITY <= real arity <= MU_UNKNOWN_ARITY
*/
#define MU_ANY_ARITY		0
#define MU_UNKNOWN_ARITY	INT_MAX

/* Macros for relational variables that name a Term. */
/* The MU_R_VAR Term associated with Z stored in the Ip table: */
#define R_VAR_INFO(Z)		((Term) KEYINFO (Ip->table, Z))

/* Associate Term T with relational variable Z: */
#define SET_R_VAR_INFO(Z,T)	KEYINFO (Ip->table, Z) = (void *) (T)

/* The name string of Z: */
#define R_VAR_NAME(Z)		KEYSTR (Ip->table, Z)

/* The BDD interpretation of the Term bound to Z: */
#define R_VAR_VALUE(Z)		T_BDD (R_VAR_INFO (Z))

/* The binding stack of Z (list of BDDs): */
#define R_VAR_BINDINGS(Z)	T_R_BINDINGS (R_VAR_INFO (Z))

/* The Term originally defined (let) for Z: */
#define R_VAR_TERM(Z)		T_R_TERM (R_VAR_INFO (Z))

/* Is T = Z? and is there a binding for Z? */
#define R_VAR_BOUND_P(T)	(   T_TYPE (T) == MU_R_VAR \
				 && R_VAR_BINDINGS (T_R_VAR (T)))

/* Macros for boolean variables. */
#define B_VAR_INFO(i)		((Formula) KEYINFO (signature->table, i))
#define SET_B_VAR_INFO(i,F)	KEYINFO (signature->table, i) = (void *) (F)
#define B_VAR_NAME(i)		KEYSTR (signature->table, i)
#define B_VAR_NAME_LEN(i)	KEYLEN (signature->table, i)

/* ------------------------------------------------------------------------ */
/* LOCAL TYPE DEFINITIONS                                                   */
/* ------------------------------------------------------------------------ */

/* Table of boolean variables. */
struct _Signature {
  int dim;
  HASHTAB *table;
};

/* Table of relational variables. */
struct _R_Interpret {
  HASHTAB *table;		/* defined info entries are Terms */
};

struct _Formula {
  FormulaType type;
  union {
    int var;			/* index in signature table */
    struct {
      Formula sub;
    } unary;
    struct {
      Formula left;
      Formula right;
    } binary;
    struct {
      Formula cond;
      Formula then_part;
      Formula else_part;
    } ite;
    struct {
      /*int*/LIST vars;		/* vars are all distinct BDD indices */
      Formula sub;
    } quantification;
    struct {
      Term term;
      /*Formula*/LIST subs;
    } application;
    struct {
      /*Formula*/LIST subs;
    } one_of;
    struct {
      /*Formula*/LIST subs;
    } none_of;
    struct {
      Formula f;
      int var;			/* index in signature table */
      Formula g;
    } subst;
  } kind;
  BDDPTR val;
  Formula next;
};

struct _Term {
  TermType type;
  int arity;
  union {
    struct {
      int var;			/* index in Ip table */
      /*BDDPTR*/LIST bindings;	/* binding stack for mu/nu R var values */
      Term def;			/* for let defined R vars; not yet used */
    } R_var;
    struct {
      Term sub;
    } unary;
    struct {
      Term left;
      Term right;
    } binary;
    struct {
      /*int*/LIST vars;		/* vars are all distinct BDD indices */
      Formula f;
    } abstraction;
    struct {
      int var;			/* index in Ip table */
      Term term;
      unsigned int other_bound_vars : 1;
				/* flag; 1 indicates this fixed-point term
				   depends on R vars that are bound by other
				   outer fixed-point terms. */
      unsigned int iter_bound : 31;
				/* bound on fixed-point iteration: will do
				   at most iter_bound iterations. */
    } fixed_point;
    struct {
      Term N;			/* interpreted as next-state relation N(x,y) */
      Term S0;			/* interpreted as initial states set */
      Term Inv;			/* interpreted as invariant to hold */
    } reach;
  } kind;
  BDDPTR val;			/* Cached BDD for this term or BDD_VOID
				   when free R vars are present. */
  Term next;
};

/* ------------------------------------------------------------------------ */
/* LOCAL VARIABLES                                                          */
/* ------------------------------------------------------------------------ */

static char SccsId[] = "%Z%%Y%/%M% %I% %G%";

static _Formula _FALSE_FORMULA = { MU_FALSE, 0 };
static _Formula _TRUE_FORMULA  = { MU_TRUE , 0 };
static    _Term _FALSE_TERM    = { MU_T_FALSE, 0 };
static    _Term _TRUE_TERM     = { MU_T_TRUE , 0 };

static const Formula MU_False_Formula = &_FALSE_FORMULA;
static const Formula MU_True_Formula  = &_TRUE_FORMULA;
static const Term    MU_False_Term    = &_FALSE_TERM;
static const Term    MU_True_Term     = &_TRUE_TERM;

/* Indexed both by TermType and FormulaType.
   Therefore those enum types are strongly related.
*/
static BDDPTR (*const BDD_FUNCTIONS[])(BDDPTR, BDDPTR) = {
  bdd_and,	 /* MU_AND,	  MU_T_AND     */
  bdd_or,	 /* MU_OR,	  MU_T_OR      */
  bdd_constrain, /* MU_COFACTOR,  MU_T_DUMMY1  */
  bdd_implies,	 /* MU_IMPLIES,	  MU_T_IMPLIES */
  bdd_equiv,	 /* MU_EQUIV,	  MU_T_EQUIV   */
  bdd_xor	 /* MU_XOR,	  MU_T_XOR     */
};

static FILE *mu_output_stream = 1; /* stdout; */

static const char *const OP_SYMBOLS[] = {
  "&",				/* MU_AND, MU_T_AND */
  "+",				/* MU_OR, MU_T_OR */
  "|",				/* MU_COFACTOR, MU_T_DUMMY1 */
  "->",				/* MU_IMPLIES, MU_T_IMPLIES */
  "==",				/* MU_EQUIV, MU_T_EQUIV */
  "xor",			/* MU_XOR, MU_T_XOR */
  "0",				/* MU_FALSE, MU_T_FALSE */
  "1",				/* MU_TRUE, MU_T_TRUE */
  NULL,
  "~"				/* MU_NOT, MU_T_NOT (prefix op) */
};

static const char precedences[] = {
  5,				/* MU_AND, MU_T_AND */
  4,				/* MU_OR, MU_T_OR */
  3,				/* MU_COFACTOR, MU_T_DUMMY1 */
  2,				/* MU_IMPLIES, MU_T_IMPLIES */
  2,				/* MU_EQUIV, MU_T_EQUIV */
  2,				/* MU_XOR, MU_T_XOR */
  0,				/* MU_FALSE, MU_T_FALSE */
  0,				/* MU_TRUE, MU_T_TRUE */
  0,
  1,				/* MU_NOT, MU_T_NOT (prefix op) */

  0,				/* MU_ITE, MU_ABSTRACT */
  0,				/* MU_EXIST, MU_L_FIXED_POINT */
  0,				/* MU_DIFF, MU_G_FIXED_POINT */
  0,				/* MU_UNIV, MU_REACH */
  0,				/* MU_APPLY, - */
  0,				/* MU_ONE_OF, - */
  0,				/* MU_NONE_OF, - */
  0				/* MU_SUBST, - */
};

static const char *const TERM_TYPES[] = {
  "And",			/* MU_T_AND */
  "Or",				/* MU_T_OR */
  "?",				/* MU_T_DUMMY1 */
  "Implies",			/* MU_T_IMPLIES */
  "Equiv",			/* MU_T_EQUIV */
  "Xor",			/* MU_T_XOR */
  "False",			/* MU_T_FALSE */
  "True",			/* MU_T_TRUE */
  "Var",
  "Not",			/* MU_T_NOT (prefix op) */
  "Lambda",
  "Mu",
  "Nu",
  "Reach"
};

static const char *const FORMULA_TYPES[] = {
  "And",			/* MU_AND */
  "Or",				/* MU_OR */
  "Cofactor",			/* MU_COFACTOR */
  "Implies",			/* MU_IMPLIES */
  "Equiv",			/* MU_EQUIV */
  "Xor",			/* MU_XOR */
  "False",			/* MU_FALSE */
  "True",			/* MU_TRUE */
  "Var",
  "Not",			/* MU_T_NOT (prefix op) */
  "Ite",
  "Exist",
  "Diff",
  "Univ",
  "Apply",
  "One",
  "None",
  "Subst"
};

/* Nr. of non-trivial Formula/Term cache hits: */
static int mu_cache_hits = 0;

static int MU_PACKAGE_INITIALIZED = 0;

/* ------------------------------------------------------------------------ */
/* EXPORTED VARIABLES                                                       */
/* ------------------------------------------------------------------------ */

Signature signature = NULL;
R_Interpret      Ip = NULL;

int mu_simplify_frontier = 0;
int mu_use_and_smooth = 0;
int mu_use_restrict = 0;
int mu_use_constrain = 0;
int mu_verbose = 0;
int mu_debug = 0;
int mu_echo = 0;

/* ------------------------------------------------------------------------ */
/* LOCAL FUNCTION PROTOTYPES                                                */
/* ------------------------------------------------------------------------ */

static void my_print_cube_action (int index, int neg, int first);
static void mu_print_term_infix_1 (Term t, int win);
static void mu_print_formula_infix_1 (Formula f, int win);

/* ************************************************************************ */
/* FUNCTION DOCUMENTATION:                                                  */
/*                                                                          */
/* ************************************************************************ */

/* BDDs for Terms are expressed over a set of dummy variables.
   A Term can be seen as a function L x1,x2, ...,xn. expr(x1,x2,...,xn).
   For ease of operating on Terms all user introduced variables x1, x2, etc.
   are mapped to dummy variables $0, $1, etc.
   Thus internally a Term is represented as:
      L $0,$1,...,$n-1. expr ($0,$1,...,$n-1), with n the arity of the Term.
   The "expr" is represented by a BDD; $0 gets BDD index 1, $1 gets 3, etc.
   The dummy or place-holder variables are thus given odd BDD indices.
   Place-holder variables are anonymous: they have no corresponding name string
   in a symbol table.
   All user variables will be allocated even BDD indices starting from 0.
   This way we can keep them apart. Note also that given the BDD (representing
   a Term) it is to a certain degree possible to interpret it as a Term and
   in fact reconstruct it in the form: L $0,$1,...,$n-1. expr ($0,$1,...,$n-1).
   One way to do so is determine the support of the BDD, and collect all the
   variables with odd indices; the even ones correspond to free (unbound)
   variables. However, the arity cannot be determined for certain; the best
   guess would be to take BDD_IDX_2_PHV_ID of the highest odd index + 1.
   It is well possible that the arity of the originating Term is different
   because some variables may have cancelled out and therefore do not appear
   in the true support of the function.

   User variables are also given an id which is their index in the signature
   hash table. These id's are consecutive numbers starting at 1.
   In the Formula and Term data structures, boolean variables are usually
   represented by this id with the exceptions of quantification and
   abstraction where the variables are represented as a list of their BDD
   indices.
*/

/* Returns the name string of a boolean variable with id var_id. */
const char *mu_bool_var_name (int var_id)
{
  return B_VAR_NAME (var_id);
}

/* Used in print_list. */
static void print_var (FILE *fp, void *bdd_idx)
{
  fputs (B_VAR_NAME (BDD_IDX_2_VAR_ID ((int) bdd_idx)), fp);
}

/* ------------------------------------------------------------------------ */
/* FORMULA/TERM ALLOCATION & FREE                                           */
/* ------------------------------------------------------------------------ */

static Formula all_formulas = NULL;
static Formula temp_formula;
static const _Formula null_formula = {0};

#define CALLOC_FORMULA() \
	( \
	 all_formulas ? (temp_formula = all_formulas, \
 		         all_formulas = F_NEXT (all_formulas), \
		        *temp_formula = null_formula, \
   		         temp_formula) \
	 : (CALLOC_STRUCT (_Formula)) \
	)

#define FREE_FORMULA(f) \
	do { \
	  Formula _xyz_f = f; \
          \
          F_NEXT (_xyz_f) = all_formulas; \
	  all_formulas = _xyz_f; \
	} once

static Term all_terms = NULL;
static Term temp_term;
static const _Term null_term = {0};

#define CALLOC_TERM() \
	( \
	 all_terms ? (temp_term = all_terms, \
 		      all_terms = T_NEXT (all_terms), \
		     *temp_term = null_term, \
   		      temp_term) \
	 : (CALLOC_STRUCT (_Term)) \
	)

#define FREE_TERM(t) \
	do { \
	  Term _xyz_t = t; \
          \
          T_NEXT (_xyz_t) = all_terms; \
	  all_terms = _xyz_t; \
	} once

/* ------------------------------------------------------------------------ */
/* RELATIONAL VARIABLE BINDING                                              */
/* ------------------------------------------------------------------------ */

/* Creates a new (as yet undefined) binding for relational variable Z.
   Any globally assigned BDD values (or previously bound ones) to R vars
   are kept on the Term's bdd field.
   This routine will move that value (if any) to the bindings list.
   Now it is safe to assign a new BDD value to its bdd field.
*/
static void create_binding (int Z)
{
  R_VAR_BINDINGS (Z) = push_cont ((void *)R_VAR_VALUE (Z), R_VAR_BINDINGS (Z));
}

/* Undoes the current binding for Z, i.e., pops and moves top-most binding
   stack value to R var's bdd field.
   Original value stored there is first saved and then returned.
   When binding stack was empty, BDD_VOID becomes the new current value.
*/
static BDDPTR undo_binding (int Z)
{
  BDDPTR old_val = R_VAR_VALUE (Z);

  R_VAR_VALUE (Z) = (BDDPTR) pop_cont (&R_VAR_BINDINGS (Z));

  return old_val;
}

/* ------------------------------------------------------------------------ */
/* MU INTERPRETER FUNCTIONS                                                 */
/* ------------------------------------------------------------------------ */

/* Calculate T = mu Z. term or T = nu Z. term. 
   Note: mu Z.Z == 0, nu Z.Z == 1.

   Note:
   Cannot do frontier set simplification unconditionally.
   Will go wrong for instance in case term uses iterative squaring.
   Our Z conceptually represents a set of paths, and NOT a set of states!
   There is a special mu_reachable function for faster reachability analysis.
*/
static BDDPTR mu_fixed_point (Term T, R_Interpret Ip)
{
  TermType  type = T_TYPE (T);
  int          Z = T_FP_VAR (T);
  Term      term = T_FP_TERM (T);
  unsigned int k = T_FP_ITER_BOUND (T);
  BDDPTR      Ti = (type == MU_L_FIXED_POINT) ? bdd_0 () : bdd_1 ();
  BDDPTR Tiplus1;
  int          i;

  if (mu_verbose) {
    fprintf (stdout, "Starting %s fixed-point calculation",
	     type == MU_L_FIXED_POINT ? "least" : "greatest");
    fflush (stdout);
  }

  create_binding (Z);

  for (i = 0; i < k; i++) {
    R_VAR_VALUE (Z) = Ti;	/* Z := Ti */

    Tiplus1 = mu_interpret_term (term, Ip, /* parent term = */ T);

    if (BDD_EQUAL_P (Tiplus1, Ti)) { /* Convergence! */
      undo_binding (Z);
      bdd_free (Tiplus1);
      if (mu_verbose)
	fprintf (stdout, "\nFixed-point found in %d steps.\n", i);
      return Ti;
    }

#ifdef DO_CHECKS
    /* Check for monotonicity:
       type == MU_L_FIXED_POINT then Ti's must be non-decreasing
       type == MU_G_FIXED_POINT then Ti's must be non-increasing
       [ Could have been checked syntactically by requiring Z to occur
         within even number of complementations. ]
    */
    if ((type == MU_L_FIXED_POINT) != BDD_COVERS (Tiplus1, Ti)) {
      fputs ("Fixed point term is not monotonic.\n", stderr);
      undo_binding (Z);
      bdd_free (Tiplus1);
      return Ti;
    }
#endif

    bdd_free (Ti);
    Ti = Tiplus1;
    if (mu_verbose) {
      putc ('.', stdout);
      fflush (stdout);
    }

/*  fprintf (stderr, "size(Ti) = %d\n", bdd_size (Ti));*/
  } /*for*/

  undo_binding (Z);
  if (mu_verbose) {
    fprintf (stdout, "\nFixed-point iteration bound reached after %d steps.\n",
	     i);
    fflush (stdout);
  }
  return Ti;
}

/* Pre: arity(S0) = n, arity(N) = 2n, n > 0.
   Returns [ mu Z . S0 + [ L y. E x. Z(x) & N(x,y) ]].

   Pseudo-code:

   eta(S) = { y | E x elem S . N(x,y) }
          = L y . E x . S(x) & N(x,y)

   STATES explore (S0, eta)
   {
     k:=0;  Front:=Tk:=S0;
     do {
       Tk+1:=Tk + eta(Front);
       if (Tk+1=Tk) return Tk;
       k++;
       Choose Front such that Tk+1-Tk <= Front <= Tk+1;
       Tk:=Tk+1;
     } forever;
   }

   or, choosing Front = Tk+1:

   STATES explore (S0, eta)
   {
     k:=0;  Tk:=S0;
     do {
       Tk+1:=S0 + eta(Tk);
       if (Tk+1=Tk) return Tk;
       k++;
       Tk:=Tk+1;
     } forever;
   }
*/
static BDDPTR mu_reachable_aux (BDDPTR N, BDDPTR S0, BDDPTR Inv, int n)
{
  BDDPTR *x_bdds = MALLOC_ARRAY (n, BDDPTR);
  LIST y_list = NULL_LIST;
  LIST x_list = NULL_LIST;
  int k = 0;
  int i;
  BDDPTR    Tk = bdd_assign (S0);
  BDDPTR Front = bdd_assign (Tk);

  if (mu_verbose)
    fprintf (stdout, "Starting Reachable calculation:\n");

  /* x_list = (     $0 ,     $1 , ...,     $n-1  ),
     y_list = (     $n ,   $n+1 , ...,    $2n-1  ),
     x_bdds = [ BDD($0), BDD($1), ..., BDD($n-1) ]
  */
  /* Make sure var ids are allocated: */
/*  bdd_free (bdd_create_var (PHV_ID_2_BDD_IDX (2 * n - 1)));*/

  for (i = 0; i < n; i++) {
    int x = PHV_ID_2_BDD_IDX (  i);
    int y = PHV_ID_2_BDD_IDX (n+i);

    x_list    = append_cont ((void *) x, x_list);
    y_list    = append_cont ((void *) y, y_list);
    x_bdds[i] = bdd_create_var (x);

    /* For a quick substitute it would be nice to have the x vars close
       in rank ordering to the y vars.
    */
/*    bdd_merge_var_groups (x, y);*/
    bdd_reorder_var (x, y);
  }

  forever {
    BDDPTR Next, Tkplus1, tmp;

    if (mu_verbose) {
      fprintf (stdout, "%10d", k);
      fflush (stdout);
    }

    /* Front <= Inv must hold: */
    if (!BDD_COVERS (Inv, Front)) {
      if (mu_verbose)
	fprintf (stdout,
		 "\nInvariant violated during Reachable calculation.\n");
      bdd_free (Front);
      bdd_free (Tk);
      Tk = bdd_0 ();
      break;
    }

    /* f(y) = E x . Front(x) & N(x,y) */
    if (mu_use_and_smooth)
      /* bdd_rank_order_vars destructively sorts, but that doesn't harm us. */

      /* bdd_and_smooth expects vars in increasing order of rank. */
      tmp = bdd_and_smooth (Front, N, x_list = bdd_rank_order_vars (x_list));
    else {
      BDDPTR tmp2;

      if (mu_use_restrict)
	tmp2 = bdd_restrict (N, Front);
      else
      if (mu_use_constrain)
	tmp2 = bdd_constrain (N, Front);
      else
	tmp2 = bdd_and (N, Front);

      /* bdd_rank_order_vars destructively sorts, but that doesn't harm us. */
      /* bdd_quantify expects vars in increasing order of rank. */
      tmp = bdd_quantify (1, tmp2, x_list = bdd_rank_order_vars (x_list));
      bdd_free (tmp2);
    }
    bdd_free (Front);

    /* Next = L y . tmp */
    Next = bdd_subst_par (x_bdds, y_list, tmp);
    bdd_free (tmp);
    k++;

/*    if (mu_simplify_frontier)*/
      Tkplus1 = bdd_or (Next, Tk);
/*
    else
      Tkplus1 = bdd_or (Next, S0);
*/
    /* monotonicity is trivially achieved: Tkplus1 >= Tk. */
    bdd_free (Next);

    if (BDD_EQUAL_P (Tkplus1, Tk)) { /* Convergence! */
      bdd_free (Tkplus1);
      if (mu_verbose)
	fprintf (stdout, "\nFixed-point found in %d steps.\n", k);
      break;
    }


    /* Can choose any Front such that Tkplus1 - Tk <= Front <= Tkplus1. */
    if (mu_simplify_frontier) {
      BDDPTR Tknot = bdd_not (Tk);
      int size_Tkplus1;
      int size_Front;

      size_Tkplus1 = bdd_size (Tkplus1);

      if (mu_use_restrict)
	Front = bdd_restrict (Tkplus1, Tknot);
      else
      if (mu_use_constrain)
	Front = bdd_constrain (Tkplus1, Tknot);
      else
	Front = bdd_and (Tkplus1, Tknot);

      size_Front = bdd_size (Front);

      bdd_free (Tknot);

      if (size_Front < size_Tkplus1) {
	bdd_free (Tk);
	Tk = Tkplus1;
	if (mu_verbose) {
	  fputs ("+", stdout);
	  fflush (stdout);
	}
	continue;
      }
      bdd_free (Front);
    }
    bdd_free (Tk);
    Tk = Tkplus1;
    Front = bdd_assign (Tk);
    if (mu_verbose) {
      fputs (" ", stdout);
      fflush (stdout);
    }
  } /*forever*/

  /* Cleanup: */
  free_list (x_list, 0);
  free_list (y_list, 0);
  bdd_free_vec (x_bdds, n);
  MA_FREE_ARRAY (x_bdds, n, BDDPTR);

/*  bdd_undo_var_groups ();*/

  return Tk;
}

static BDDPTR mu_reachable (Term T, R_Interpret Ip, Term FT)
{
  clock_t start_t = clock ();

  Term   T_N = T_REACH_N (T);
  Term  T_S0 = T_REACH_S0 (T);
  Term T_Inv = T_REACH_INV (T);
  int   T_N_arity = T_ARITY (T_N);
  int           n = T_ARITY (T_S0);
  int T_Inv_arity;
  BDDPTR S0 = mu_interpret_term (T_S0, Ip, FT);
  BDDPTR N, R, Inv;

  Inv = mu_interpret_term (T_Inv, Ip, FT);
  T_Inv_arity = BDD_CONST_P (Inv) ? n : T_ARITY (T_Inv);

  if (BDD_CONST_P (S0)) {
    /* 0 or 1; not expecting any X's. */
    /* May unconditionally cache result: */
    if (BDD_COVERS (Inv, S0)) {
      bdd_free (Inv);
      R = S0;
#ifdef USE_CACHE
      T_BDD (T) = bdd_assign (R);
#endif
      goto done;
    }
    bdd_free (S0);
    bdd_free (Inv);
    R = bdd_0 ();
#ifdef USE_CACHE
    T_BDD (T) = bdd_assign (R);
#endif
    goto done;
  }

  N = mu_interpret_term (T_N , Ip, FT);

  if (BDD_0_P (N)) {
    bdd_free (N);
    /* 0 or 1; not expecting any X's. */
    /* May unconditionally cache result: */
    if (BDD_COVERS (Inv, S0)) {
      bdd_free (Inv);
      R = S0;
#ifdef USE_CACHE
      T_BDD (T) = bdd_assign (R);
#endif
      goto done;
    }
    bdd_free (S0);
    bdd_free (Inv);
    R = bdd_0 ();
#ifdef USE_CACHE
    T_BDD (T) = bdd_assign (R);
#endif
    goto done;
  }

  if (BDD_1_P (N)) {
    bdd_free (S0);
    /* 0 or 1; not expecting any X's. */
    /* May unconditionally cache result: */
    if (BDD_1_P (Inv)) {
      bdd_free (Inv);
      R = N;
#ifdef USE_CACHE
      T_BDD (T) = bdd_assign (R);
#endif
      goto done;
    }
    bdd_free (N);
    bdd_free (Inv);
    R = bdd_0 ();
#ifdef USE_CACHE
    T_BDD (T) = bdd_assign (R);
#endif
    goto done;
  }

  if (    T_N_arity  == MU_UNKNOWN_ARITY
      ||           n == MU_UNKNOWN_ARITY
      ||  T_N_arity  == MU_ANY_ARITY
      ||           n == MU_ANY_ARITY
      ||  T_N_arity  != MUL2 (n)
      || T_Inv_arity == MU_UNKNOWN_ARITY
      || T_Inv_arity != n) {
    yyerror ("Bad arities in Reachable calculation");
    bdd_free (N);
    bdd_free (S0);
    bdd_free (Inv);
    R = T_BDD (T) = BDD_VOID;
    goto done;
  }

  R = mu_reachable_aux (N, S0, Inv, n);
  bdd_free (N);
  bdd_free (S0);
  bdd_free (Inv);

#ifdef USE_CACHE
  if (   !BDD_VOID_P (T_BDD (T_REACH_N (T)))
      && !R_VAR_BOUND_P (T_REACH_N (T))
      && !BDD_VOID_P (T_BDD (T_REACH_S0 (T)))
      && !R_VAR_BOUND_P (T_REACH_S0 (T))
      && !BDD_VOID_P (T_BDD (T_REACH_INV (T)))
      && !R_VAR_BOUND_P (T_REACH_INV (T)))
    /* Cache result: */
    T_BDD (T) = bdd_assign (R);
#endif

 done:

  if (mu_verbose) {
    BDDPTR Domain = bdd_1 ();
    /*  char buf[36]; */		/* more than enough */
    int i;

    for (i = n - 1; i >= 0; i--) {
      BDDPTR v = bdd_create_var (PHV_ID_2_BDD_IDX (i));
      BDDPTR R = bdd_and (Domain, v);

      bdd_free (Domain);
      bdd_free (v);
      Domain = R;
    }

    /*  D_sprintf (buf, bdd_count_sat_assignments (R, Domain), 0); */
    fprintf (stdout, "Reachable took %.2f msec (%u BDD nodes).\n",
	             (clock () - start_t) / CLOCKS_PER_MSEC, 
                     bdd_size (R));
    bdd_free (Domain);
  }
  return R;
}

/* Returns BDD representation for: L vars . f
   vars is list of BDD indices.
   Neither vars nor f are modified or freed.
*/

/* Alternative: using bdd_subst_par instead of bdd_subst. */
static BDDPTR mu_abstract (LIST vars, BDDPTR f)
{
  int nr_vars = LIST_SIZE (vars);
  BDDPTR *d_bdds;
  BDDPTR R;
  int i;

  /* There could be less abstraction variables than vars in support of
     f (vars). In that case result might still depend on some free vars
     besides the dummy place-holders.
  */

  if (!nr_vars)
    return bdd_assign (f);

  /* Set up BDD vector of $i dummy variables: */
  d_bdds = MALLOC_ARRAY (nr_vars, BDDPTR);

  i = 0;
  FOR_EACH_LIST_ELEM (vars, elem) {
    int x = (int) ELEM_CONTENTS (elem);
    int d = PHV_ID_2_BDD_IDX (i);

    d_bdds[i] = bdd_create_var (d);
    /* For a quick substitute it would be nice to have the d vars close
       in rank ordering to the x vars.
    */
/*
    bdd_free (bdd_create_var (x));
    bdd_merge_var_groups (d, x);
*/
    bdd_reorder_var (d, x);
    i++;
  } END_FOR_EACH_LIST_ELEM;

  if (mu_debug) {
    i = 0;
    FOR_EACH_LIST_ELEM (vars, elem) {
      int x = (int) ELEM_CONTENTS (elem);
      int d = PHV_ID_2_BDD_IDX (i);

      fprintf (stderr, "/* %s (rank:%d) <- $%d (rank:%d) */\n", 
	       B_VAR_NAME (BDD_IDX_2_VAR_ID (x)),
	       BDD_VAR_RANK (x),
	       i,
	       BDD_VAR_RANK (d));
      i++;
    } END_FOR_EACH_LIST_ELEM;
  }

  R = bdd_subst_par (d_bdds, vars, f);
  bdd_free_vec (d_bdds, nr_vars);
  MA_FREE_ARRAY (d_bdds, nr_vars, BDDPTR);

  /* Final term might still contain free Boolean variables. */
  return R;
}

BDDPTR mu_interpret_term (Term T, R_Interpret Ip, Term FT)
{
  BDDPTR tmp1, tmp2, R;

  if (mu_debug) {
    fprintf (stderr, "[mu_interpret_term]: %s", TERM_TYPES[T_TYPE (T)]);
    if (T_TYPE (T) == MU_R_VAR)
      fprintf (stderr, " `%s'", R_VAR_NAME (T_R_VAR (T)));
    fprintf (stderr, "\n");
  }

  /* Cannot cache result when term T contains Relational vars bound by
     an outer mu/nu fixed-point construct because these change during
     the fixed-point computation. The Term FT (if non-NULL) provides a
     link to the first surrounding fixed-point construct; via FT's next
     fields any more outer fixed-point constructs may be reached.
  */

  if (T_TYPE (T) == MU_R_VAR) {
    int var = T_R_VAR (T);

    if (BDD_VOID_P (T_BDD (T))) {
      yyerror ("Unbound R variable `%s'; interpreted as 0", R_VAR_NAME (var));
      T_BDD (T) = bdd_0 ();
    }
    else
    if (R_VAR_BINDINGS (var)) {
      /* Check all outstanding fixed-point terms FT:
	 (There must be one that binds this var and we want the most inner one)
	 (Better could have done this once as a preprocessing step on the
	 syntactic level)
      */
      while (FT) {
	if (var == T_FP_VAR (FT))
	  /* Gotcha! Here the closest fixed-point term is found that binds
	     this var. No need to look further at outer binders.
	  */
	  break;
	/* Tell this fixed-point term that it has R-vars that are bound by
	   other outer fixed-point terms. This means that we cannot cache
	   the result from this fixed-point term interpretation.
	   This flag is set once and never reset.
	*/
	T_FP_ENV (FT) = 1;

	FT = T_NEXT (FT);
      } /*while*/
    }
    /* else: Must be globally defined R var (via let). */
    return bdd_assign (T_BDD (T));
  }

  if (!BDD_VOID_P (T_BDD (T))) {
    /* Use cached result: */
    /* Don't count any trivial hits: */
    if (T_TYPE (T) != MU_T_FALSE && T_TYPE (T) != MU_T_TRUE)
      mu_cache_hits++;
    return bdd_assign (T_BDD (T));
  }

  switch (T_TYPE (T)) {
  case MU_T_FALSE:
  case MU_T_TRUE:
    /* Not possible! These always have a cached result. */
    break;

  case MU_R_VAR:
    /* Not possible! Already treated above as special case. */
    break;

  case MU_T_NOT:
    tmp1 = mu_interpret_term (T_SUB (T), Ip, FT);
    R = bdd_not (tmp1);
    bdd_free (tmp1);

#ifdef USE_CACHE
    if (   !BDD_VOID_P (T_BDD (T_SUB (T)))
	&& !R_VAR_BOUND_P (T_SUB (T)))
      /* Cache result: */
      T_BDD (T) = bdd_assign (R);
#endif
    return R;

  case MU_T_AND:
  case MU_T_OR:
  case MU_T_EQUIV:
  case MU_T_XOR:
  case MU_T_IMPLIES:
    tmp1 = mu_interpret_term (T_LEFT (T) , Ip, FT);
    tmp2 = mu_interpret_term (T_RIGHT (T), Ip, FT);

    R = BDD_FUNCTIONS[T_TYPE (T)](tmp1, tmp2);

    bdd_free (tmp1);
    bdd_free (tmp2);

#ifdef USE_CACHE
    if (   !BDD_VOID_P (T_BDD (T_LEFT (T)))
	&& !R_VAR_BOUND_P (T_LEFT (T))
	&& !BDD_VOID_P (T_BDD (T_RIGHT (T)))
	&& !R_VAR_BOUND_P (T_RIGHT (T)))
      /* Cache result: */
      T_BDD (T) = bdd_assign (R);
#endif
    return R;

  case MU_ABSTRACT:
    tmp1 = mu_interpret_formula (T_LAMBDA_F (T), Ip, FT);
    R = mu_abstract (T_LAMBDA_VARS (T), tmp1);
    bdd_free (tmp1);

#ifdef USE_CACHE
    if (!BDD_VOID_P (T_BDD (T_LAMBDA_F (T))))
      /* Cache result: */
      T_BDD (T) = bdd_assign (R);
#endif
    /* Final term might still contain free Boolean variables. */
    return R;

  case MU_L_FIXED_POINT:
  case MU_G_FIXED_POINT:
    /* Establish upwards pointer to parent via next field: */
    T_NEXT (T) = FT;
    R = mu_fixed_point (T, Ip);
    /* Reset next field: */
    T_NEXT (T) = NULL;

    /* Check whether there are any variables Z other than
       T_FP_VAR (T) that are bound by an outer mu/nu Z.
       This is the case when there are indeed outer fixed-point terms
       (FT != NULL) and this term's flag (T_FP_ENV) is set.
       If not, can cache this result.
    */
#ifdef USE_CACHE
    if (!T_FP_ENV (T))
      /* Cache result: */
      T_BDD (T) = bdd_assign (R);
#endif
    return R;

  case MU_REACH:
    return mu_reachable (T, Ip, FT);

  default:
    return BDD_VOID;
  } /*switch*/
}

/* Returns BDD representation for: t (args).
   t is BDD representing the evaluated term.
   args is a vector of BDDs, representing the evaluated arguments.
   Neither args nor t are modified or freed.
*/

/* Alternative: using bdd_subst_par instead of bdd_subst. */
static BDDPTR mu_apply (BDDPTR t, BDDPTR *args, int nr_args)
{
  LIST bdd_idxs = NULL_LIST;
  BDDPTR R;
  int i;

  if (!nr_args)
    return bdd_assign (t);

  /* Note: doing applicative order reduction, i.e.,
     first evaluate arguments and then substitute in function term.
     No currying is possible, i.e., an application must
     fully evaluate to a formula; it cannot return a term.
  */

  /* Set up list of bdd indices for place-holders: */
  if (mu_debug) {
    for (i = 0; i < nr_args; i++) {
      const char *str;

      fprintf (stderr, "/* $%d <- ", i);
      var_table = signature->table;
      str = bdd_get_output_string (BDD_END_S);
      bdd_set_output_string (BDD_END_S, " */\n");
      bdd_print_as_sum_of_cubes (stderr, args[i], 0);
      bdd_set_output_string (BDD_END_S, str);

      bdd_idxs = append_cont ((void *) PHV_ID_2_BDD_IDX (i), bdd_idxs);
    }
  }
  else
    for (i = 0; i < nr_args; i++)
      bdd_idxs = append_cont ((void *) PHV_ID_2_BDD_IDX (i), bdd_idxs);

  R = bdd_subst_par (args, bdd_idxs, t);
  free_list (bdd_idxs, 0);

  /* R is final application result; it must be a formula. */
  return R;
}

/* Destructively converts list of user-introduced var id's to a list of
   the corresponding BDD indices.
*/
static LIST mu_varids_to_bdd_indices_aux (LIST vars)
{
  FOR_EACH_LIST_ELEM (vars, elem) {
    int var_id = (int) ELEM_CONTENTS (elem);
    int bdd_idx = VAR_ID_2_BDD_IDX (var_id);

    ELEM_CONTENTS (elem) = (void *) bdd_idx;
  } END_FOR_EACH_LIST_ELEM;

  return vars;
}

/* Interpret a list of formulas; usually they are the arguments in an
   application.
   The resulting BDDs are recorded in f_bdds, which must have been
   allocated as a vector of BDD pointers of the proper size.
   Returns 1 when all formulas evaluate correctly, else 0.
   The Term FT (if non-NULL) provides a link to the first surrounding
   fixed-point construct. This is used to determine whether certain
   intermediate results can be cached.
*/
static int mu_interpret_formula_list (LIST f_list, BDDPTR *f_bdds,
				      R_Interpret Ip, Term FT)
{
  int all_evaluated = 1;

  /* Interpret all arguments and keep in args: */
  FOR_EACH_LIST_ELEM (f_list, elem) {
    Formula argi = (Formula) ELEM_CONTENTS (elem);

    *f_bdds++ = mu_interpret_formula (argi, Ip, FT);

    all_evaluated &= !BDD_VOID_P (F_BDD (argi));

  } END_FOR_EACH_LIST_ELEM;

  return all_evaluated;
}

/* Returns BDD representation for: E vars . f & g
   vars is list of BDD indices.
   Neither vars nor f nor g are modified or freed.
*/
static BDDPTR mu_and_smooth (LIST vars, BDDPTR f, BDDPTR g)
{
  BDDPTR R;
  /* bdd_rank_order_vars destructively sorts hence need of copy: */
  LIST bdd_vars = copy_list (vars, 0);

  /* bdd_and_smooth expects vars in increasing order of rank. */
  R = bdd_and_smooth (f, g, bdd_vars = bdd_rank_order_vars (bdd_vars));
  free_list (bdd_vars, 0);

  return R;
}

/* Returns BDD representation for: Q vars . f
   vars is list of BDD indices.
   Neither vars nor f are modified or freed.
*/
static BDDPTR mu_quantify (int Q, LIST vars, BDDPTR f)
{
  BDDPTR R;
  /* bdd_rank_order_vars destructively sorts hence need of copy: */
  LIST bdd_vars = copy_list (vars, 0);

  /* bdd_quantify expects vars in increasing order of rank. */
  R = bdd_quantify (Q, f, bdd_vars = bdd_rank_order_vars (bdd_vars));
  free_list (bdd_vars, 0);

  return R;
}

/* Returns BDD representation for: D vars . f
   D means boolean difference.
   vars is list of BDD indices.
   Neither vars nor f are modified or freed.
*/
static BDDPTR mu_diff (LIST vars, BDDPTR f)
{
  BDDPTR R;
  LIST bdd_vars = copy_list (vars, 0);

  /* bdd_diff expects vars in increasing order of rank. */
  R = bdd_diff (f, bdd_vars = bdd_rank_order_vars (bdd_vars));
  free_list (bdd_vars, 0);

  return R;
}

BDDPTR mu_interpret_formula (Formula f, R_Interpret Ip, Term FT)
{
  BDDPTR tmp1, tmp2, R;

  if (mu_debug) {
    fprintf (stderr, "[mu_interpret_formula]: %s", FORMULA_TYPES[F_TYPE (f)]);
    if (F_TYPE (f) == MU_B_VAR)
      fprintf (stderr, " `%s'", B_VAR_NAME (F_VAR (f)));
    fprintf (stderr, "\n");
  }

  if (!BDD_VOID_P (F_BDD (f))) {
    /* Use cached result: */
    /* Don't count any trivial hits: */
    if (   F_TYPE (f) != MU_FALSE
	&& F_TYPE (f) != MU_TRUE
	&& F_TYPE (f) != MU_B_VAR)
      mu_cache_hits++;
    return bdd_assign (F_BDD (f));
  }

  /* Results are cached only if results of sub-formulas also have been
     cached. This way caching propagates upwards (to the root) in the formula
     tree, or might stop at some level and then BDD_VOID is recorded to
     indicate that no cached values are available.
     The latter might happen when a term contains Relational vars bound by
     an outer mu/nu fixed-point construct because these change during
     the fixed-point computation. The Term FT (if non-NULL) provides a
     link to the first surrounding fixed-point construct; via FT's next
     fields any more outer fixed-point constructs may be reached.
  */

  switch (F_TYPE (f)) {
  case MU_FALSE:
  case MU_TRUE:
  case MU_B_VAR:
    /* Not possible! These always have a cached result. */
    break;

  case MU_NOT:
    tmp1 = mu_interpret_formula (F_SUB (f), Ip, FT);
    R = bdd_not (tmp1);
    bdd_free (tmp1);

#ifdef USE_CACHE
    if (!BDD_VOID_P (F_BDD (F_SUB (f))))
      /* Cache result: */
      F_BDD (f) = bdd_assign (R);
#endif
    return R;

  case MU_AND:
  case MU_OR:
  case MU_COFACTOR:
  case MU_EQUIV:
  case MU_XOR:
  case MU_IMPLIES:
    tmp1 = mu_interpret_formula (F_LEFT (f) , Ip, FT);
    tmp2 = mu_interpret_formula (F_RIGHT (f), Ip, FT);

    R = BDD_FUNCTIONS[F_TYPE (f)](tmp1, tmp2);

    bdd_free (tmp1);
    bdd_free (tmp2);

#ifdef USE_CACHE
    if (   !BDD_VOID_P (F_BDD (F_LEFT (f)))
	&& !BDD_VOID_P (F_BDD (F_RIGHT (f))))
      /* Cache result: */
      F_BDD (f) = bdd_assign (R);
#endif
    return R;

  case MU_ITE:
    {
      BDDPTR C;

      C    = mu_interpret_formula (F_COND (f), Ip, FT);
      tmp1 = mu_interpret_formula (F_THEN (f), Ip, FT);
      tmp2 = mu_interpret_formula (F_ELSE (f), Ip, FT);

      R = bdd_ite (C, tmp1, tmp2);

      bdd_free (C);
      bdd_free (tmp1);
      bdd_free (tmp2);

#ifdef USE_CACHE
      if (   !BDD_VOID_P (F_BDD (F_COND (f)))
	  && !BDD_VOID_P (F_BDD (F_THEN (f)))
	  && !BDD_VOID_P (F_BDD (F_ELSE (f))))
	/* Cache result: */
	F_BDD (f) = bdd_assign (R);
#endif
    }
    return R;

  case MU_DIFF:
    tmp1 = mu_interpret_formula (F_QUANT_F (f), Ip, FT);
    R = mu_diff (F_QUANT_VARS (f), tmp1);
    bdd_free (tmp1);
    goto rest_of_quant;

  case MU_EXIST:
    if (mu_use_and_smooth) {
      Formula sub = F_QUANT_F (f);

      /* No cached result and a conjunction? */
      if (BDD_VOID_P (F_BDD (sub)) && F_TYPE (sub) == MU_AND) {
	/* Do an and_smooth. */
	tmp1 = mu_interpret_formula (F_LEFT (sub) , Ip, FT);
	tmp2 = mu_interpret_formula (F_RIGHT (sub), Ip, FT);

#ifdef USE_CACHE
	if (   !BDD_VOID_P (F_BDD (F_LEFT (sub)))
	    && !BDD_VOID_P (F_BDD (F_RIGHT (sub))))
	  /* Cache result: */
	  F_BDD (F_QUANT_F (f)) = bdd_and (tmp1, tmp2);
#endif
	R = mu_and_smooth (F_QUANT_VARS (f), tmp1, tmp2);
	bdd_free (tmp1);
	bdd_free (tmp2);
	goto rest_of_quant;
      }
    }
    /* FALL THROUGH. */

  case MU_UNIV:
    tmp1 = mu_interpret_formula (F_QUANT_F (f), Ip, FT);
    R = mu_quantify (F_TYPE (f) == MU_EXIST, F_QUANT_VARS (f), tmp1);
    bdd_free (tmp1);

  rest_of_quant:
#ifdef USE_CACHE
    if (!BDD_VOID_P (F_BDD (F_QUANT_F (f))))
      /* Cache result: */
      F_BDD (f) = bdd_assign (R);
#endif
    return R;

  case MU_APPLY:
    {
      int       nr_args = LIST_SIZE (F_APPLY_SUBS (f));
      BDDPTR      *args = MALLOC_ARRAY (nr_args, BDDPTR);
      int all_evaluated = mu_interpret_formula_list (F_APPLY_SUBS (f),
						     args, Ip, FT);

      tmp1 = mu_interpret_term (F_APPLY_TERM (f), Ip, FT);

      R = mu_apply (tmp1, args, nr_args);
      bdd_free (tmp1);

      bdd_free_vec (args, nr_args);
      MA_FREE_ARRAY (args, nr_args, BDDPTR);

#ifdef USE_CACHE
      if (   !BDD_VOID_P (F_BDD (F_APPLY_TERM (f)))
	  && !R_VAR_BOUND_P (F_APPLY_TERM (f))
	  && all_evaluated)
	/* Cache result: */
	F_BDD (f) = bdd_assign (R);
#endif
      /* R is final application result; it must be a formula. */
    }
    return R;

  case MU_ONE_OF:
    {
      int       nr_args = LIST_SIZE (F_ONE_OF_SUBS (f));
      BDDPTR      *args = MALLOC_ARRAY (nr_args, BDDPTR);
      int all_evaluated = mu_interpret_formula_list (F_ONE_OF_SUBS (f),
						     args, Ip, FT);

      R = bdd_one_of_vec (args, nr_args);

      bdd_free_vec (args, nr_args);
      MA_FREE_ARRAY (args, nr_args, BDDPTR);

#ifdef USE_CACHE
      if (all_evaluated)
	/* Cache result: */
	F_BDD (f) = bdd_assign (R);
#endif
    }
    return R;

  case MU_NONE_OF:
    {
      int       nr_args = LIST_SIZE (F_NONE_OF_SUBS (f));
      BDDPTR      *args = MALLOC_ARRAY (nr_args, BDDPTR);
      int all_evaluated = mu_interpret_formula_list (F_NONE_OF_SUBS (f),
						     args, Ip, FT);

      R = bdd_none_of_vec (args, nr_args);

      bdd_free_vec (args, nr_args);
      MA_FREE_ARRAY (args, nr_args, BDDPTR);

#ifdef USE_CACHE
      if (all_evaluated)
	/* Cache result: */
	F_BDD (f) = bdd_assign (R);
#endif
    }
    return R;

  case MU_SUBST:
    tmp1 = mu_interpret_formula (F_SUBST_F (f), Ip, FT);
    tmp2 = mu_interpret_formula (F_SUBST_G (f), Ip, FT);

    R = bdd_subst (tmp2, VAR_ID_2_BDD_IDX (F_SUBST_VAR (f)), tmp1);

    bdd_free (tmp1);
    bdd_free (tmp2);

#ifdef USE_CACHE
    if (   !BDD_VOID_P (F_BDD (F_SUBST_F (f)))
	&& !BDD_VOID_P (F_BDD (F_SUBST_G (f))))
      /* Cache result: */
      F_BDD (f) = bdd_assign (R);
#endif
    return R;

  default:
    return BDD_VOID;
  } /*switch*/
}

/* ------------------------------------------------------------------------ */
/* CONSTRUCTOR FUNCTIONS (mainly used by parser)                            */
/* ------------------------------------------------------------------------ */

Formula mu_mk_false_formula (void)
{
  return MU_False_Formula;
}

Formula mu_mk_true_formula (void)
{
  return MU_True_Formula;
}

Formula mu_mk_bool_var (char *name)
{
  Formula info = NULL;

  lookup (signature->table, name, strlen (name), (void **) &info, LOOKUP);

  return info;
}

Formula mu_mk_unary_formula (FormulaType type, Formula f1)
{
  Formula f = CALLOC_FORMULA ();

  F_TYPE (f) = type;
  F_SUB (f)  = f1;
  return f;
}

Formula mu_mk_binary_formula (FormulaType type, Formula f1, Formula f2)
{
  Formula f = CALLOC_FORMULA ();

  F_TYPE (f)  = type;
  F_LEFT (f)  = f1;
  F_RIGHT (f) = f2;
  return f;
}

Formula mu_mk_ite_formula (Formula cond, Formula then_part, Formula else_part)
{
  Formula f = CALLOC_FORMULA ();

  F_TYPE (f) = MU_ITE;
  F_COND (f) = cond;
  F_THEN (f) = then_part;
  F_ELSE (f) = else_part;
  return f;
}

Formula mu_mk_quantified_formula (FormulaType type, LIST vars, Formula f1)
{
  Formula f = CALLOC_FORMULA ();

  F_TYPE (f)       = type;
  F_QUANT_VARS (f) = mu_varids_to_bdd_indices_aux (vars);
  F_QUANT_F (f)    = f1;
  return f;
}

/* Pre: |subs| > 0.
   Always expects arity(R) to be known.

   Allowing circumvented way of currying, i.e., applications of functions
   with arity greater than the number of arguments.
   This must result in a term instead of a formula. Therefore parser allows
   special Term construct [ Application ] and calls mu_mk_curry on result
   of mu_mk_application.

   It is however always possible to create a term with smaller arity
   by an abstraction, e.g.:

   let f = L x,y,z. x & y + z';
   let g = L a. f(c,d,a);
   Now g is the term L a. c & d + a', with free variables c and d.
   Of course, it is also possible to bind any free variables again
   by an abstraction:
   let h = L c,d,z. g(z);
   Resulting in f = h.

   Perhaps the distinction between formulas and terms should disappear:
   1 data structure could be used for both formulas and terms with an
   indication determined at compile-time of which is which.
   Both are represented by BDD's anyway, the only difference being that
   formulas are expressed in terms of the [declared] domain variables and
   terms are expressed using dummy place-holder variables besides.

   If curried is true expects |subs| < arity(R) with the exception of arity 0.
   
   If not curried then requires |subs| = arity(R), again with exception of
   arity 0.
*/
Formula mu_mk_application (Term R, LIST subs, int curried)
{
  Formula f = CALLOC_FORMULA ();

  /* Arity of term R must be <= LIST_SIZE (subs) */
  if (T_ARITY (R) == MU_UNKNOWN_ARITY)
    yyerror ("Arity of term is unknown");
  else
  if (T_ARITY (R) > LIST_SIZE (subs)) {
    /* T_ARITY (R) != MU_ANY_ARITY */
    if (!curried)
      yyerror ("Too few arguments in application");
  }
  else
  if (T_ARITY (R) == LIST_SIZE (subs)) {
    /* T_ARITY (R) != MU_ANY_ARITY */
    if (curried)
      yyerror ("No currying possible: application results in formula");
  }
  else
  if (T_ARITY (R) && T_ARITY (R) < LIST_SIZE (subs)) {
    yywarning ("Too many arguments; excess ones are dropped");
    if (curried)
      yyerror ("No currying possible: application results in formula");
  }

  F_TYPE (f)        = MU_APPLY;
  F_APPLY_TERM (f)  = R;
  F_APPLY_SUBS (f)  = subs;
  return f;
}

Formula mu_mk_one_of (LIST subs)
{
  Formula f = CALLOC_FORMULA ();

  F_TYPE (f)        = MU_ONE_OF;
  F_ONE_OF_SUBS (f) = subs;
  return f;
}

Formula mu_mk_none_of (LIST subs)
{
  Formula f = CALLOC_FORMULA ();

  F_TYPE (f)         = MU_NONE_OF;
  F_NONE_OF_SUBS (f) = subs;
  return f;
}

/* Substitute g for var in f. */
Formula mu_mk_subst (Formula f, int var, Formula g)
{
  Formula F = CALLOC_FORMULA ();

  F_TYPE (F)      = MU_SUBST;
  F_SUBST_F (F)   = f;
  F_SUBST_VAR (F) = var;
  F_SUBST_G (F)   = g;
  return F;
}

static char *itoa (char *buf, int n)
{
  sprintf (buf, "%d", n);
  return buf;
}

static int combine_arities (Term T1, Term T2)
{
  int T1_arity = T_ARITY (T1);
  int T2_arity = T_ARITY (T2);
  char buf1[16];
  char buf2[16];

  if (T1_arity && T2_arity && T1_arity != T2_arity)
    yywarning ("Arities of terms mismatch: %s against %s",
	       T1_arity == MU_UNKNOWN_ARITY ? "?" : itoa (buf1, T1_arity),
	       T2_arity == MU_UNKNOWN_ARITY ? "?" : itoa (buf2, T2_arity));

  /* Take the maximum: */
  return T1_arity > T2_arity ? T1_arity : T2_arity;
}

Term mu_mk_false_term (void)
{
  return MU_False_Term;
}

Term mu_mk_true_term (void)
{
  return MU_True_Term;
}

int mu_mk_rel_var_dcl (char *name)
{
  int index;
  int flag;

  flag = (int) INSERT;
  index = lookup (Ip->table, name, strlen (name), NULL, &flag);
  if (flag == INDEED_INSERTED) {
    Term  T = CALLOC_STRUCT (_Term);

    T_TYPE (T)  = MU_R_VAR;
    T_ARITY (T) = MU_ANY_ARITY;
    T_R_VAR (T) = index;
    SET_R_VAR_INFO (index, T);

    if (mu_verbose)
      fprintf (stdout, "Added Relational variable `%s' to Ip.\n", name);
  }
/*
  else
    yywarning ("R variable `%s' shadows earlier definition", name);
*/
  return index;
}

Term mu_mk_rel_var (R_Interpret Ip, char *name)
{
  int index;
  int flag;

  if (mu_verbose)
    fprintf (stdout, "Looking up Relational variable: `%s'.\n", name);

  flag = (int) INSERT;
  index = lookup (Ip->table, name, strlen (name), NULL, &flag);
  if (flag == INDEED_INSERTED) {
    Term  T = CALLOC_TERM ();

    T_TYPE (T)  = MU_R_VAR;
    T_ARITY (T) = MU_UNKNOWN_ARITY;
    T_R_VAR (T) = index;
    SET_R_VAR_INFO (index, T);

    yywarning ("R variable `%s' has no defined value", name);
  }
  return R_VAR_INFO (index);
}

/* Check for possible eta-reduction:
   L vars. FT = L vars . FA (vars) ==> FA.
   Return Term FA if successful, otherwise NULL.
*/
static Term try_eta_reduction (LIST vars, Formula FT)
{
  if (F_TYPE (FT) == MU_APPLY) {
    /* L vars . FA (a1,a2,a?) */
    int nr_vars = LIST_SIZE (vars);
    LIST   args = F_APPLY_SUBS (FT);
    int nr_args = LIST_SIZE (args);

    if (nr_vars == nr_args) {
      /* L vars . FA (a1,a2,...,a-nr_vars) */
      LIST_ELEM_PTR lvars = LIST_FIRST (vars);

      FOR_EACH_LIST_ELEM (args, elem) {
	Formula  argi = (Formula) ELEM_CONTENTS (elem);
	int     lvari = (int) ELEM_CONTENTS (lvars);

	if (F_TYPE (argi) != MU_B_VAR || F_VAR (argi) != lvari)
	  /* vi != argi */
	  return NULL;

	lvars = LIST_NEXT (lvars);
      } END_FOR_EACH_LIST_ELEM;
      /* Here L vars . FA (vars). */
      return F_APPLY_TERM (FT);
    }
  }
  return NULL;
}

static Term mu_mk_abstraction_aux (LIST vars, Formula f1)
{
  Term t = CALLOC_TERM ();

  T_TYPE (t)        = MU_ABSTRACT;
  T_ARITY (t)       = LIST_SIZE (vars);	/* >= 1 */
  T_LAMBDA_VARS (t) = mu_varids_to_bdd_indices_aux (vars);
  T_LAMBDA_F (t)    = f1;

  return t;
}

Term mu_mk_abstraction (LIST vars, Formula f1)
{
  Term t;

  /* Check for possible eta-reduction:
     L vars . f (vars) ==> f
  */
  if (t = try_eta_reduction (vars, f1)) {
    /* Not creating an abstraction type of Term, therefore: */
    free_list (vars, 0);
    /* Avoid freeing the Term t: */
    F_APPLY_TERM (f1) = NULL;
    /* Now can safely free the formula: */
    mu_free_formula (f1);
  }
  else
    t = mu_mk_abstraction_aux (vars, f1);

  return t;
}

Term mu_mk_curry (Formula FT)
{
  LIST   args = F_APPLY_SUBS (FT);
  int nr_args = LIST_SIZE (args);
  Term     FA = F_APPLY_TERM (FT);
  int   arity = T_ARITY (FA);

  if (arity > nr_args) {
    int i;
    LIST vars = NULL_LIST;

    for (i = 0; i < arity - nr_args; i++) {
      char buf[16];
      int var_id;

      /* Create new user variable: */
      sprintf (buf, "#%d", i);
      var_id = mu_check_bool_var (buf);

      /* Create list of var id's for L <varid's> . */
      vars = append_cont ((void *) var_id, vars);
      /* Add as Formula to args: */
      args = append_cont ((void *) B_VAR_INFO (var_id), args);
    }

    F_APPLY_SUBS (FT) = args;

    return mu_mk_abstraction_aux (vars, FT);
  }
  mu_free_formula (FT);

  return mu_mk_false_term ();
}

Term mu_mk_fixed_point (TermType type, R_Interpret Ip, int var, Term R,
			unsigned int iter_bound)
{
  Term t = CALLOC_TERM ();

  T_TYPE (t)    = type;
  T_ARITY (t)   = T_ARITY (R);
  /* R_VAR_INFO (var)->arity */
  T_FP_VAR (t)  = var;
  T_FP_TERM (t) = R;
  T_FP_ITER_BOUND (t) = iter_bound;
  return t;
}

/* Reachable(Next,S0) == mu Z . S0 + [ L y . E x . Z(x) & Next(x,y)].
   Inv is invariant that should hold for all reachable states;
   if not, then result will be 0 term.
*/
Term mu_mk_reach (Term Next, Term S0, Term Inv)
{
  Term t = CALLOC_TERM ();

  T_TYPE (t)      = MU_REACH;
  T_REACH_N (t)   = Next;
  T_REACH_S0 (t)  = S0;
  T_REACH_INV (t) = Inv;
  T_ARITY (t)     = T_ARITY (S0);

  if (T_ARITY (Next) == MU_UNKNOWN_ARITY)
    yyerror ("Arity of first arg to Reachable is unknown");
  else
  if (T_ARITY (S0) == MU_UNKNOWN_ARITY)
    yyerror ("Arity of second arg to Reachable is unknown");
  else
  if (T_ARITY (Inv) == MU_UNKNOWN_ARITY)
    yyerror ("Arity of third arg to Reachable is unknown");
  else
  if (T_ARITY (Next) && T_ARITY (S0) && T_ARITY (Next) != MUL2(T_ARITY (S0)))
    yyerror ("Arity of first arg to Reachable must be twice second arg");

  return t;
}

Term mu_mk_unary_term (TermType type, Term T1)
{
  Term T = CALLOC_TERM ();

  T_TYPE (T)  = type;
  T_ARITY (T) = T_ARITY (T1);
  T_SUB (T)   = T1;
  return T;
}

Term mu_mk_binary_term (TermType type, Term T1, Term T2)
{
  Term T = CALLOC_TERM ();

  T_TYPE (T)  = type;
  T_ARITY (T) = combine_arities (T1, T2);
  T_LEFT (T)  = T1;
  T_RIGHT (T) = T2;
  return T;
}

void mu_mk_let (int var, Term T)
{
  clock_t start_t = clock ();

  Term     old_def = R_VAR_TERM (var);
  const char *name = R_VAR_NAME (var);
  BDDPTR         R = R_VAR_VALUE (var);

  /* Get rid of any old definition: */
  if (old_def)
    mu_free_term (old_def);

  /* Get rid of any current BDD value: */
  if (!BDD_VOID_P (R)) {
    bdd_free (R);
    yywarning ("Redefining R variable `%s'", name);
  }

  if (mu_verbose) {
    fprintf (stdout, "Interpreting defining term for R variable `%s'...",
	     name);
    fflush (stdout);
  }

  R_VAR_VALUE (var) = R = mu_interpret_term (T, Ip, NULL);

  if (mu_verbose)
    fprintf (stdout, "done (%d BDD nodes).\n", bdd_size (R));

  /* Inherit arity of defining term: */
  T_ARITY (R_VAR_INFO (var)) = T_ARITY (T);

  if (T_ARITY (T) == MU_UNKNOWN_ARITY)
    yyerror ("R variable `%s's definition has unknown arity", name);

  if (mu_echo) {
    fprintf (stdout, "let %s = ", name);
    mu_print_term_infix (stdout, T);
    fputs (";\n", stdout);
  }

  /* Save definition: */
  /* Don't see any applications yet. */
/*  R_VAR_TERM (var) = T;*/
  mu_free_term (T);

/*
  {
    Term T = mu_BDD_2_Term (R);

    mu_print_term_infix (stdout, T);
    fputs (";\n", stdout);
    mu_free_term (T);
  }
*/

  bdd_dynamic_order_exhaustive ();

  if (mu_verbose)
    fprintf (stdout, "Definition for `%s' took %d msec (%d BDD nodes).\n",
	     name, (clock () - start_t) / CLOCKS_PER_MSEC,
	     bdd_size (R));
}

void mu_init (void)
{
  /* Guard against multiple initialisations: */
  if (MU_PACKAGE_INITIALIZED) {
    if (mu_verbose)
      fprintf (stdout, "[mu_init]: Package already initialized.\n");
    return;
  }

  if (mu_verbose)
    fprintf (stdout,
"[mu_init]: v1.4 Copyright (C) 1992-1997 G. Janssen, Eindhoven University\n");

  signature = CALLOC_STRUCT (_Signature);

  signature->table = hashtab_create (0);

  _FALSE_FORMULA.val = bdd_0 ();
  _TRUE_FORMULA.val  = bdd_1 ();
  _FALSE_TERM.val    = bdd_0 ();
  /* Constant terms assume any arity: */
  _FALSE_TERM.arity  = MU_ANY_ARITY;
  _TRUE_TERM.val     = bdd_1 ();
  _TRUE_TERM.arity   = MU_ANY_ARITY;

  /* We do not use entry nr. 0: */
  lookup (signature->table, "", 0, NULL, INSERT);

  /* Change style of bdd_sum_of_cubes printing: */
  bdd_set_output_string (BDD_BEG_S, "");
  bdd_set_output_string (BDD_END_S, ";\n");
  bdd_set_output_string (BDD_AND_S, " & ");
  bdd_set_output_string (BDD_OR_S, " + ");

  bdd_print_cube_action = my_print_cube_action;

  signature->dim = 0;

  Ip = CALLOC_STRUCT (_R_Interpret);
  Ip->table = hashtab_create (0);

  MU_PACKAGE_INITIALIZED = 1;
}

void mu_quit (void)
{
  /* Guard against quit without init (includes multiple quits): */
  if (!MU_PACKAGE_INITIALIZED) {
    if (mu_verbose)
      fprintf (stdout, "[mu_quit]: Package not initialized.\n");
    return;
  }

  /* Delete all user boolean variables: */
  FOR_EACH_OCCUPIED_BUCKET (signature->table, i) {
    Formula F = B_VAR_INFO (i);

    if (F) {
      bdd_free (F_BDD (F));
      FREE_FORMULA (F);
    }
  } END_FOR_EACH_OCCUPIED_BUCKET;

  free_hashtab (signature->table);
  MA_FREE_STRUCT (signature, _Signature);

  signature = NULL;

  bdd_free (_FALSE_FORMULA.val);
  bdd_free (_TRUE_FORMULA.val);
  bdd_free (_FALSE_TERM.val);
  bdd_free (_TRUE_TERM.val);

  /* Delete all user relational variables: */
  FOR_EACH_OCCUPIED_BUCKET (Ip->table, i) {
    Term T = R_VAR_INFO (i);

    if (T) {
      bdd_free (T_BDD (T));
      free_list (T_R_BINDINGS (T), (void (*) (void *)) bdd_free);
      mu_free_term (T_R_TERM (T));
      FREE_TERM (T);
    }
  } END_FOR_EACH_OCCUPIED_BUCKET;

  free_hashtab (Ip->table);
  MA_FREE_STRUCT (Ip, _R_Interpret);

  Ip = NULL;

  while (all_formulas) {
    temp_formula = F_NEXT (all_formulas);
    MA_FREE_STRUCT (all_formulas, _Formula);
    all_formulas = temp_formula;
  }

  while (all_terms) {
    temp_term = T_NEXT (all_terms);
    MA_FREE_STRUCT (all_terms, _Term);
    all_terms = temp_term;
  }

  mu_cache_hits = 0;

  MU_PACKAGE_INITIALIZED = 0;
}

void mu_mk_signature (LIST vars)
{
  int dim = signature->dim;

  FOR_EACH_LIST_ELEM (vars, elem) {
    char *name = ELEM_CONTENTS (elem);
    int index;
    int flag;

    flag = (int) INSERT;
    index = lookup (signature->table, name, strlen (name), NULL, &flag);
    if (flag == INDEED_INSERTED) {
      Formula f = CALLOC_FORMULA ();

      F_TYPE (f) = MU_B_VAR;
      F_VAR (f)  = index;
      F_BDD (f)  = bdd_create_var (VAR_ID_2_BDD_IDX (index));
      SET_B_VAR_INFO (index, f);
      dim++;
    }
    else
      yywarning ("Variable `%s' already declared; skipped", name);

  } END_FOR_EACH_LIST_ELEM;

  signature->dim = dim;

  free_list (vars, free);
}

int mu_check_bool_var (char *var)
{
  int index;
  int flag;

  flag = (int) INSERT;
  index = lookup (signature->table, var, strlen (var), NULL, &flag);
  if (flag == INDEED_INSERTED) {
    Formula f = CALLOC_FORMULA ();

    F_TYPE (f) = MU_B_VAR;
    F_VAR (f)  = index;
    /* Make sure rank table knows about this variable: */
    F_BDD (f)  = bdd_create_var (VAR_ID_2_BDD_IDX (index));
    SET_B_VAR_INFO (index, f);
    signature->dim++;

    yywarning ("Variable `%s' was not declared before", var);
  }
  return index;
}

/* List of BDD variable id's corresponding to user introduced B vars. */
static LIST user_vars;

/* p is in fact BDD variable id. If odd it refers to a place-holder variable,
   else it's a user variable which is appended to the list user_vars.
*/
static int when_even (void *p)
{
  if (ODD ((int) p))
    return 0;
  user_vars = append_cont (p, user_vars);
  return 1;
}

static Formula cube_as_formula;

static void mk_formula_cube_action (int index, int neg, int first)
{
  Formula v = B_VAR_INFO (BDD_IDX_2_VAR_ID (index));

  if (neg)
    v = mu_mk_unary_formula (MU_NOT, v);

  if (first)
    cube_as_formula = v;
  else
    cube_as_formula = mu_mk_binary_formula (MU_AND, cube_as_formula, v);
}

/* Given the BDD (supposedly representing a Formula) interpret it as a Formula
   and reconstruct it (in sum-of-cubes form for now).
*/
Formula mu_BDD_2_Formula (BDDPTR f)
{
  LIST cubes;
  BDDPTR cube;
  Formula cubes_as_formula;

  if (BDD_VOID_P (f))
    return NULL;

  if (BDD_0_P (f) || BDD_X_P (f))
    return mu_mk_false_formula ();

  if (BDD_1_P (f))
    return mu_mk_true_formula ();

  cubes = bdd_sum_of_cubes_as_list (f);

  cube = (BDDPTR) pop_cont (&cubes);
  bdd_traverse_cube (cube, mk_formula_cube_action);
  bdd_free (cube);
  cubes_as_formula = cube_as_formula;

  while (cube = (BDDPTR) pop_cont (&cubes)) {
    bdd_traverse_cube (cube, mk_formula_cube_action);
    bdd_free (cube);
    cubes_as_formula = mu_mk_binary_formula (MU_OR, cubes_as_formula,
					     cube_as_formula);
  }
  return cubes_as_formula;
}

/* Given the BDD (supposedly representing a Term) interpret it as a Term
   and reconstruct it in the form: L $0,$1,...,$n-1. expr ($0,$1,...,$n-1).
   The arity cannot be determined for certain; as best guess we take
   BDD_IDX_2_PHV_ID of the highest odd index + 1 of a support variable.
*/
Term mu_BDD_2_Term (BDDPTR f)
{
  LIST vars;

  if (BDD_VOID_P (f))
    return NULL;

  if (BDD_0_P (f) || BDD_X_P (f))
    return mu_mk_false_term ();

  if (BDD_1_P (f))
    return mu_mk_true_term ();

  /* List of var id's in increasing order of their ranks: */
  vars = bdd_support_as_list_of_vars (f);

  /* Remove all the user variables and collect them in user_vars: */
  user_vars = NULL_LIST;

  if (vars = remove_elements (vars, when_even, 0, 0)) {
    /* Here only place-holder variables with id >= 1 left. */
    int max_idx = 0;
    int idx;
    int i;
    Formula F;
    int nr_args;
    BDDPTR *args;
    BDDPTR R;
    LIST bdd_idxs = NULL_LIST;

    /* Get maximum place-holder BDD index: */
    while (idx = (int) pop_cont (&vars))
      if (idx > max_idx) max_idx = idx;
    /* Here: vars == NULL_LIST */

    /* Determine minimum arity for Term: */
    nr_args = BDD_IDX_2_PHV_ID (max_idx) + 1;

    args = MALLOC_ARRAY (nr_args, BDDPTR);

    for (i = 0; i < nr_args; i++) {
      char buf[16];
      int var_id;

      /* Create new user variable: */
      sprintf (buf, "#%d", i);
      var_id   = mu_check_bool_var (buf);

      /* Create list of var id's for L <varid's> . */
      vars     = append_cont ((void *) var_id, vars);
      /* vars as BDDs: */
      args[i]  = F_BDD (B_VAR_INFO (var_id));
      /* Create list of corresponding BDD indices: */
      bdd_idxs = append_cont ((void *) PHV_ID_2_BDD_IDX (i), bdd_idxs);
    }

    /* Substitute lambda vars for place-holder vars in Term's BDD: */
    R = bdd_subst_par (args, bdd_idxs, f);
    MA_FREE_ARRAY (args, nr_args, BDDPTR);
    free_list (bdd_idxs, 0);

    /* Convert Term's BDD to a Formula: */
    F = mu_BDD_2_Formula (R);

    bdd_free (R);
    free_list (user_vars, 0);

    return mu_mk_abstraction_aux (vars, F);
  }

  /* No place-holder variables present in Term's BDD. */
  free_list (user_vars, 0);

  return NULL;
}

/* ------------------------------------------------------------------------ */
/* DESTRUCTOR FUNCTIONS                                                     */
/* ------------------------------------------------------------------------ */

void mu_free_formula (Formula f)
{
  if (!f) return;

  switch (F_TYPE (f)) {
  case MU_FALSE:
  case MU_TRUE:
  case MU_B_VAR:
    return;

  case MU_NOT:
    mu_free_formula (F_SUB (f));
    break;

  case MU_AND:
  case MU_OR:
  case MU_COFACTOR:
  case MU_EQUIV:
  case MU_XOR:
  case MU_IMPLIES:
    mu_free_formula (F_LEFT (f));
    mu_free_formula (F_RIGHT (f));
    break;

  case MU_ITE:
    mu_free_formula (F_COND (f));
    mu_free_formula (F_THEN (f));
    mu_free_formula (F_ELSE (f));
    break;

  case MU_EXIST:
  case MU_DIFF:
  case MU_UNIV:
    free_list (F_QUANT_VARS (f), 0);
    mu_free_formula (F_QUANT_F (f));
    break;

  case MU_APPLY:
    mu_free_term (F_APPLY_TERM (f));
    free_list (F_APPLY_SUBS (f), (void (*) (void *)) mu_free_formula);
    break;

  case MU_ONE_OF:
    free_list (F_ONE_OF_SUBS (f), (void (*) (void *)) mu_free_formula);
    break;

  case MU_NONE_OF:
    free_list (F_NONE_OF_SUBS (f), (void (*) (void *)) mu_free_formula);
    break;

  case MU_SUBST:
    mu_free_formula (F_SUBST_F (f));
    mu_free_formula (F_SUBST_G (f));
    break;

  default:
    return;
  } /*switch*/
  bdd_free (F_BDD (f));
  FREE_FORMULA (f);
}

void mu_free_term (Term T)
{
  if (!T) return;

  switch (T_TYPE (T)) {
  case MU_T_FALSE:
  case MU_T_TRUE:
    /* These are constant Terms. */
    return;

  case MU_R_VAR:
    /* This R var may be refered to by other terms, so keep it. */
    return;

  case MU_T_NOT:
    mu_free_term (T_SUB (T));
    break;

  case MU_T_AND:
  case MU_T_OR:
  case MU_T_EQUIV:
  case MU_T_XOR:
  case MU_T_IMPLIES:
    mu_free_term (T_LEFT (T));
    mu_free_term (T_RIGHT (T));
    break;

  case MU_ABSTRACT:
    mu_free_formula (T_LAMBDA_F (T));
    free_list (T_LAMBDA_VARS (T), 0);
    break;

  case MU_L_FIXED_POINT:
  case MU_G_FIXED_POINT:
    mu_free_term (T_FP_TERM (T));
    break;

  case MU_REACH:
    mu_free_term (T_REACH_N (T));
    mu_free_term (T_REACH_S0 (T));
    break;

  default:
    return;
  } /*switch*/
  bdd_free (T_BDD (T));
  FREE_TERM (T);
}

/* ------------------------------------------------------------------------ */
/* PRINT FUNCTIONS                                                          */
/* ------------------------------------------------------------------------ */

static void my_print_cube_action (int index, int neg, int first)
{
  char *name;

  if (ODD (index)) {
    /* Think up dummy name, say $<index>, but no check is made whether
       this name already exists as a user introduced variable!
    */
    static char buf[16];

    sprintf (buf, "$%d", BDD_IDX_2_PHV_ID (index));
    name = buf;
  }
  else
    name = B_VAR_NAME (BDD_IDX_2_VAR_ID (index));

  if (!first) {
    putc (' ', bdd_output_stream);
    fputs (OP_SYMBOLS[MU_AND], bdd_output_stream);
    putc (' ', bdd_output_stream);
  }
  fputs (name, bdd_output_stream);
  if (neg)
    putc ('\'', bdd_output_stream);
}

/* Used in print_list. */
static void print_formula (FILE *fp, void *f)
{
  mu_print_formula_infix_1 ((Formula) f, 0);
}

static void mu_print_formula_infix_1 (Formula f, int win)
{
  Formula X, Y;
  int wout, type;

  if (!f) return;

  type = F_TYPE (f);
  wout = precedences[type];

  switch (type) {
  case MU_FALSE:
  case MU_TRUE:
    fputs (OP_SYMBOLS[type], mu_output_stream);
    break;

  case MU_B_VAR:
    fputs (B_VAR_NAME (F_VAR (f)), mu_output_stream);
    break;

  case MU_NOT:
    f = F_SUB (f);
    if (type == MU_B_VAR) {
      fputs (B_VAR_NAME (F_VAR (f)), mu_output_stream);
      putc ('\'', mu_output_stream);
    }
    else {
      fputs (OP_SYMBOLS[MU_NOT], mu_output_stream);
      mu_print_formula_infix_1 (f, 6);
    }
    break;

  case MU_AND:
  case MU_OR:
  case MU_COFACTOR:
  case MU_EQUIV:
  case MU_XOR:
  case MU_IMPLIES:

    X = F_LEFT (f);
    Y = F_RIGHT (f);

    if (wout < win) putc ('(', mu_output_stream);
    mu_print_formula_infix_1 (X, wout);
    putc (' ', mu_output_stream);
    fputs (OP_SYMBOLS[type], mu_output_stream);
    putc (' ', mu_output_stream);
    mu_print_formula_infix_1 (Y, wout);
    if (wout < win) putc (')', mu_output_stream);
    break;

  case MU_ITE:
    if (1 < win) putc ('(', mu_output_stream);

    mu_print_formula_infix_1 (F_COND (f), 0);

    putc ('?', mu_output_stream);

    mu_print_formula_infix_1 (F_THEN (f), 1);

    putc (':', mu_output_stream);

    mu_print_formula_infix_1 (F_ELSE (f), 1);

    if (1 < win) putc (')', mu_output_stream);
    break;

  case MU_EXIST:
  case MU_DIFF:
  case MU_UNIV:
    if (1 < win) putc ('(', mu_output_stream);

    switch (type) {
    case MU_EXIST:
      putc ('E', mu_output_stream);
      break;
    case MU_DIFF:
      putc ('D', mu_output_stream);
      break;
    case MU_UNIV:
      putc ('A', mu_output_stream);
      break;
    }

    print_list (mu_output_stream, " ", F_QUANT_VARS (f), print_var, ",", "");

    putc ('.', mu_output_stream);

    mu_print_formula_infix_1 (F_QUANT_F (f), 0);

    if (1 < win) putc (')', mu_output_stream);
    break;

  case MU_APPLY:
    mu_print_term_infix_1 (F_APPLY_TERM (f), 100);

    print_list (mu_output_stream, "(", F_APPLY_SUBS (f),
		print_formula, ",", ")");
    break;

  case MU_ONE_OF:
    print_list (mu_output_stream, "one_of(", F_ONE_OF_SUBS (f),
		print_formula, ",", ")");
    break;

  case MU_NONE_OF:
    print_list (mu_output_stream, "none_of(", F_NONE_OF_SUBS (f),
		print_formula, ",", ")");
    break;

  case MU_SUBST:
    if (1 < win) putc ('(', mu_output_stream);

    mu_print_formula_infix_1 (F_SUBST_F (f), 0);

    putc ('[', mu_output_stream);

    fputs (B_VAR_NAME (F_SUBST_VAR (f)), mu_output_stream);

    putc (':', mu_output_stream);
    putc ('=', mu_output_stream);

    mu_print_formula_infix_1 (F_SUBST_G (f), 0);

    putc (']', mu_output_stream);

    if (1 < win) putc (')', mu_output_stream);
    break;

  default:
    return;
  } /*switch*/
}

static void mu_print_term_infix_1 (Term t, int win)
{
  Term X, Y;
  int wout, type;

  if (!t) return;

  type = T_TYPE (t);
  wout = precedences[type];

  switch (type) {
  case MU_T_FALSE:
  case MU_T_TRUE:
    fputs (OP_SYMBOLS[type], mu_output_stream);
    break;

  case MU_R_VAR:
    fputs (R_VAR_NAME (T_R_VAR (t)), mu_output_stream);
    break;

  case MU_T_NOT:
    t = T_SUB (t);
    if (type == MU_R_VAR) {
      fputs (R_VAR_NAME (T_R_VAR (t)), mu_output_stream);
      putc ('\'', mu_output_stream);
    }
    else {
      fputs (OP_SYMBOLS[MU_T_NOT], mu_output_stream);
      mu_print_term_infix_1 (t, 5);
    }
    break;

  case MU_T_AND:
  case MU_T_OR:
  case MU_T_EQUIV:
  case MU_T_XOR:
  case MU_T_IMPLIES:

    X = T_LEFT (t);
    Y = T_RIGHT (t);

    if (wout < win) putc ('[', mu_output_stream);
    mu_print_term_infix_1 (X, wout);
    putc (' ', mu_output_stream);
    fputs (OP_SYMBOLS[type], mu_output_stream);
    putc (' ', mu_output_stream);
    mu_print_term_infix_1 (Y, wout);
    if (wout < win) putc (']', mu_output_stream);
    break;

  case MU_ABSTRACT:
    if (1 < win) putc ('[', mu_output_stream);

    putc ('L', mu_output_stream);

    print_list (mu_output_stream, " ", T_LAMBDA_VARS (t), print_var, ",", "");

    putc ('.', mu_output_stream);

    mu_print_formula_infix_1 (T_LAMBDA_F (t), 0);

    if (1 < win) putc (']', mu_output_stream);
    break;

  case MU_L_FIXED_POINT:
  case MU_G_FIXED_POINT:
    if (1 < win) putc ('[', mu_output_stream);

    if (type == MU_L_FIXED_POINT)
      putc ('m', mu_output_stream);
    else
      putc ('n', mu_output_stream);
    putc ('u', mu_output_stream);

    if (T_FP_ITER_BOUND (t) < INT_MAX)
      fprintf (mu_output_stream, " (%d)", T_FP_ITER_BOUND (t));

    putc (' ', mu_output_stream);
    fputs (R_VAR_NAME (T_FP_VAR (t)), mu_output_stream);

    putc ('.', mu_output_stream);

    mu_print_term_infix_1 (T_FP_TERM (t), 0);

    if (1 < win) putc (']', mu_output_stream);
    break;

  case MU_REACH:
    fputs ("Reachable", mu_output_stream);
    putc ('(', mu_output_stream);
    mu_print_term_infix_1 (T_REACH_N (t), 0);
    putc (',', mu_output_stream);
    mu_print_term_infix_1 (T_REACH_S0 (t), 0);
    if (T_REACH_INV (t) != MU_True_Term) {
      putc (',', mu_output_stream);
      mu_print_term_infix_1 (T_REACH_INV (t), 0);
    }
    putc (')', mu_output_stream);
    break;

  default:
    return;
  } /*switch*/
}

void mu_print_formula_infix (FILE *fp, Formula f)
{
  mu_output_stream = fp;
  mu_print_formula_infix_1 (f, 0);
}

void mu_print_term_infix (FILE *fp, Term t)
{
  mu_output_stream = fp;
  mu_print_term_infix_1 (t, 0);
}

void mu_print_stats (FILE *fp)
{
  fprintf (fp, "*** MU Package Statistics ***\n");
  fprintf (fp, "Formula/Term Cache hits: %d.\n", mu_cache_hits);
}
