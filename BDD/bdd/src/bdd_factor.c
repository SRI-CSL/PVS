/*
 DOCUMENTATION INFORMATION				         module: BDD
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system	   : HP 9000/S500
 file	   : bdd_factor.c
 unit-title: 
 ref.	   :
 author(s) : Copyright (c) 1994-1995 Jeroen Soede, Koen van Eijk, Geert Janssen
 date	   : 30-JUN-1995
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

#include "bdd.h"
#include "bdd_extern.h"
#include "bdd_factor.h"

/* ------------------------------------------------------------------------ */
/* LOCAL DEFINES                                                            */
/* ------------------------------------------------------------------------ */

/* Nr. bits allocated for inedge counter per BDD node.
   Counter saturates at MAXINEDGECOUNT value.
*/
#define NR_INEDGE_COUNT_BITS     5
#define MAXINEDGECOUNT           (((unsigned int) 1 << NR_INEDGE_COUNT_BITS)-1)
/* Nr. bits allocated for left/right sub sizes per node.
   Counter saturates at MAXFACTORSIZE value.
*/
#define NR_SIZE_BITS             ((32-3-NR_INEDGE_COUNT_BITS) >> 1)
#define MAXFACTORSIZE            (((unsigned int) 1 << NR_SIZE_BITS)-1)
/* Nr. bits allocated for sub expression id. */
#define NR_INDEX_BITS            (32-3)

/*
   The macro BDD_FACTOR_CAST1 is used to interpret BDD_AUX1_L as
   the structure factor1_aux. Similarly, the macro BDD_FACTOR_CAST2 interprets
   BDD_AUX1_L as the structure factor2_aux.

   As these structures are both stored in BDD_AUX1_L, some caution should be
   taken. The first 3 bits should be defined in exactly the same order,
   so that these bits can be accessed uniformly in both structures.
*/

#define BDD_FACTOR_CAST1(F)     ((factor1_aux *) &BDD_AUX1_L (F))
#define BDD_FACTOR_CAST2(F)     ((factor2_aux *) &BDD_AUX1_L (F))

/* The following 3 macros can be used for both factor1_aux and factor2_aux: */
#define BDD_SUBEXPR_INV(F)      (BDD_FACTOR_CAST1 (F)->subexpr_inv)
#define BDD_ROOT_FLAG(F)        (BDD_FACTOR_CAST1 (F)->root_flag)
#define BDD_SUBEXPR_FLAG(F)     (BDD_FACTOR_CAST1 (F)->subexpr_flag)

/* The following 3 macros can only be used for factor1_aux: */
#define BDD_INEDGE_COUNT(F)     (BDD_FACTOR_CAST1 (F)->inedge_cnt)
#define BDD_POS_SIZE(F)         (BDD_FACTOR_CAST1 (F)->pos_size)
#define BDD_NEG_SIZE(F)         (BDD_FACTOR_CAST1 (F)->neg_size)

/* The following macro can only be used for factor2_aux: */
#define BDD_SUBEXPR_INDEX(F)    (BDD_FACTOR_CAST2 (F)->subexpr_index)

/* Saturating increment of inedge counter: */
#define BDD_INCR_INCOUNT(F)     do { \
				  if (BDD_INEDGE_COUNT (F) < MAXINEDGECOUNT) \
				     BDD_INEDGE_COUNT (F)++; \
			        } while (0)

/* macro for using a function of the current_interface */
#define USE(FUNCTION_NAME)      (current_interface->FUNCTION_NAME)

#define OUT_FP			(current_interface->out)

/* ------------------------------------------------------------------------ */
/* LOCAL TYPE DEFINITIONS                                                   */
/* ------------------------------------------------------------------------ */

/* These struct precisely overlay the 32-bit AUX1 field in a BDD node. */
typedef struct {
  unsigned int subexpr_inv   :  1;
  unsigned int root_flag     :  1;
  unsigned int subexpr_flag  :  1;
  unsigned int inedge_cnt    :  NR_INEDGE_COUNT_BITS;
  unsigned int pos_size      :  NR_SIZE_BITS;
  unsigned int neg_size      :  NR_SIZE_BITS;
} factor1_aux;

typedef struct {
  unsigned int subexpr_inv   :  1;
  unsigned int root_flag     :  1;
  unsigned int subexpr_flag  :  1;
  unsigned int subexpr_index :  NR_INDEX_BITS;
} factor2_aux;

/* ------------------------------------------------------------------------ */
/* LOCAL FUNCTION PROTOTYPES                                                */
/* ------------------------------------------------------------------------ */

static void print_const (BDDPTR v);
static void print_lit (int v, int negated);
static void print_or (void);
static void print_and (void);
static void print_xor (void);
static void print_left (int op, int context);
static void print_right (int op, int context);
static void print_sub_index (int v, int negated);
static void print_root_index (int v, int negated);
static void print_begin_expr (int v);
static void print_end_expr (void);
static void print_begin_root (int v);
static void print_end_root (void);
static void print_root_exprs (int count);
static void print_sub_exprs (int count);

/* ------------------------------------------------------------------------ */
/* LOCAL VARIABLES                                                          */
/* ------------------------------------------------------------------------ */
static char SccsId[] = "%Z%%Y%/%M% %I% %G%";

static int count;		/* counter used in naming the index */

static bdd_factor_interface bdd_default_factor_interface =
{
  1, /* stdout, */
  print_const,
  print_lit,
  print_or,
  print_and,
  print_xor,
  print_left,
  print_right,
  print_sub_index,
  print_root_index,
  print_begin_expr,
  print_end_expr,
  print_begin_root,
  print_end_root,
  print_root_exprs,
  print_sub_exprs,
  1,    /* Use Koen's order in printing [sub-]expressions. */
  1,    /* Use xors 1: yes */
  1,    /* Implication Check: yes */
  3,    /* Highest used array index: maxI */
  /* Each pair (#inedges, #size) of following entries means:
     Create new subexpression if number of references to this BDD node
     is at least #inedges and the number of literals in the expression
     as printed is at least #size.
     Pairs are checked in order as given here; (0,0) pair of course
     always succeeds.
  */
  1,8,
  2,4,
  3,3,
  4,2,  /* Last used pair of the matrix A */
  0,0, 0,0  /* Unused array elements */
};

/* Init the current interface: */
static bdd_factor_interface *current_interface = &bdd_default_factor_interface;

/* ************************************************************************ */
/* FUNCTION DOCUMENTATION:                                                  */
/*                                                                          */
/* ************************************************************************ */

static void print_const (BDDPTR v)
{
  if BDD_VOID_P (v)
    fputs ("void", OUT_FP);
  else
  if BDD_0_P (v)
    fputc ('0', OUT_FP);
  else
  if BDD_1_P (v)
    fputc ('1', OUT_FP);
  else
  if BDD_X_P (v)
    fputc ('X', OUT_FP);
}

static void print_lit (int v, int negated)
{
  /* Don't know relation between BDD var id and user name for it;
     therefore simply use var id value directly like "v2", etc.
  */
  fprintf (OUT_FP, "v%d%s", v, negated ? "'": "");
}

static void print_or (void)
{
  fputs (" + ", OUT_FP);
}

static void print_and (void)
{
  /* Like to have AND operation implicit. */
  fputc (' ', OUT_FP);
}

static void print_xor (void)
{
  fputs (" * ", OUT_FP);
}

static void print_left (int op, int context)
{
  /* Here operator and context values are assumed to coincide with
     their precedence, i.e., an OR operator inside a AND context will
     indeed require enclosing in parentheses.
  */
  if (op < context)
    fputc ('(', OUT_FP);
}

static void print_right (int op, int context)
{
  /* Here operator and context values are assumed to coincide with
     their precedence, i.e., an OR operator inside a AND context will
     indeed require enclosing in parentheses.
  */
  if (op < context)
    fputc (')', OUT_FP);
}

static void print_sub_index (int v, int negated)
{
  /* Don't know how user wants to name subexpression;
     Just using "E[i]" where i is the internal id for a subexpression.
  */
  fprintf (OUT_FP, "E[%d]%s", v, negated ? "'": "");
}

static void print_root_index (int v, int negated)
{
  /* Don't know how user wants to name top-level expression;
     Just using "F[i]" where i is the internal id for the expression.
  */
  fprintf (OUT_FP, "F[%d]%s", v, negated ? "'": "");
}

static void print_begin_expr (int v)
{
  fputs ("let ", OUT_FP);
  print_sub_index (v, 0);
  fputs (" = ", OUT_FP);
}

static void print_end_expr (void)
{
  fputs (".\n", OUT_FP);
}

static void print_begin_root (int v)
{
  fputs ("let ", OUT_FP);
  print_root_index (v, 0);
  fputs (" = ", OUT_FP);
}

static void print_end_root (void)
{
  fputs (".\n", OUT_FP);
}

static void print_root_exprs (int count)
{
  fprintf (OUT_FP, "\n/* %d Top-level expression%s: */\n\n",
	   count, count != 1 ? "s" : "");
}

static void print_sub_exprs (int count)
{
  fprintf (OUT_FP, "\n/* %d Subexpression%s: */\n\n",
	   count, count != 1 ? "s" : "");
}

/* ************************************************************************ */
/*                                                                          */
/* ************************************************************************ */

/* General purpose pre-order traversal over vector of BDDs. */
static void bdd_traverse_vec_pre (BDDPTR *F, int vec_size,
				  void (*pre_action) (BDDPTR))
{
  BDDPTR marked_node = BDD_VOID;
  int k;

  for (k = 0; k < vec_size; k++)
    if (!BDD_VOID_P (F[k])) {
      if (BDD_VOID_P (marked_node) || BDD_NOT_MARKED (F[k], marked_node))
	bdd_traverse_pre (F[k], pre_action);
      marked_node = F[k];
    }
}

/* General purpose post-order traversal over vector of BDDs. */
static void bdd_traverse_vec_post (BDDPTR *F,int vec_size,
				   void (*post_action) (BDDPTR))
{
  BDDPTR marked_node = BDD_VOID;
  int k;

  for (k = 0; k < vec_size; k++)
    if (!BDD_VOID_P (F[k])) {
      if (BDD_VOID_P (marked_node) || BDD_NOT_MARKED (F[k],marked_node))
	bdd_traverse_post (F[k], post_action);
      marked_node = F[k];
    }
}

/* Handle an expression using subexpressions and root names. */
/* Top-level call will always have print_flag = 1.
   When called recursively print_flag will always be 0.
*/
static void bdd_handle_aux (BDDPTR f, int context, int print_flag)
{
  int varid;
  BDDPTR  T;
  BDDPTR  E;

  if (BDD_TERM_P (f)) {
    USE (handle_const) (f);
    return;
  }

  /* Stop at a node with index > 0 (subexpression), unless this is the
     top node of the expression. This is indicated by print_flag. */

  if (!print_flag && BDD_SUBEXPR_FLAG (f)) {
    int negated = BDD_NEG_P (f) != BDD_SUBEXPR_INV (f);

    if (BDD_ROOT_FLAG (f))
      USE (handle_root_index) (BDD_SUBEXPR_INDEX (f), negated);
    else
      USE (handle_sub_index) (BDD_SUBEXPR_INDEX (f), negated);

    return;
  }

  varid = BDD_VARID (f);
  T     = BDD_COFACTOR_POS (f);
  E     = BDD_COFACTOR_NEG (f);

  if (BDD_LIT_P (f))
    USE (handle_lit) (varid, BDD_0_P (T));
  else
  if (BDD_0_P (T)) {
    /* v 0 + v' Expr --> v' Expr */
    USE (handle_left) (BDD_AND_OP, context);
    USE (handle_lit) (varid, 1);
    USE (handle_and) ();
    bdd_handle_aux (E, BDD_AND_OP, 0);
    USE (handle_right) (BDD_AND_OP, context);
  }
  else
  if (BDD_0_P (E)) {
    /* v Expr + v 0 --> v Expr */
    USE (handle_left) (BDD_AND_OP, context);
    USE (handle_lit) (varid, 0);
    USE (handle_and) ();
    bdd_handle_aux (T, BDD_AND_OP, 0);
    USE (handle_right) (BDD_AND_OP, context);
  }
  else
  if (PTR (T) == PTR (E) && current_interface->use_xors) {
    /* Detected an exclusive OR:
       v Expr + v' Expr'= v * Epxr'
       v Expr'+ v' Expr = v * Expr
    */
    USE (handle_left) (BDD_XOR_OP, context);
    USE (handle_lit) (varid, 0);
    USE (handle_xor) ();
    bdd_handle_aux (E, BDD_XOR_OP, 0);
    USE (handle_right) (BDD_XOR_OP, context);
  }
  else {
    /* General situation, also looking for implications and 1 edges :
       Implication checks : v T + v' E and T->E then T + v' E
                            v T + v' E and E->T then v T + E
       1 edges            : v 1 + v' Expr --> v + Epxr
                            v Expr + v' 1 --> Expr + v'
    */
    USE (handle_left) (BDD_OR_OP, context);
    if (   BDD_1_P (E) /* trivial case */
	|| current_interface->use_impl_check && BDD_IMPLIES_TAUT (T, E))
      bdd_handle_aux (T, BDD_OR_OP, 0);
    else {
      if (!BDD_1_P (T)) {
	USE (handle_left) (BDD_AND_OP, BDD_OR_OP);
	USE (handle_lit) (varid, 0);
	USE (handle_and) ();
	bdd_handle_aux (T, BDD_AND_OP, 0);
	USE (handle_right) (BDD_AND_OP, BDD_OR_OP);
      }
      else
	USE (handle_lit) (varid, 0);
    }
    USE (handle_or) ();
    if (   BDD_1_P (T) /* trivial case */
	|| current_interface->use_impl_check && BDD_IMPLIES_TAUT (E, T))
      bdd_handle_aux (E, BDD_OR_OP, 0);
    else {
      if (!BDD_1_P (E)) {
	USE (handle_left) (BDD_AND_OP, BDD_OR_OP);
	USE (handle_lit) (varid, 1);
	USE (handle_and) ();
	bdd_handle_aux (E, BDD_AND_OP, 0);
	USE (handle_right) (BDD_AND_OP, BDD_OR_OP);
      }
      else
	USE (handle_lit) (varid, 1);
    }
    USE (handle_right) (BDD_OR_OP, context);
  }
}

/*
   Handles f with bdd_handle_aux, if f is a subexpression or a root.
   The expression is expressed such that the number of negative literals
   is minimal.
*/
static void print_action (BDDPTR f)
{
  if (!BDD_TERM_P (f) && BDD_SUBEXPR_FLAG (f) && !BDD_ROOT_FLAG (f)) {
    USE (handle_begin_expr) (BDD_SUBEXPR_INDEX (f));

    if (BDD_SUBEXPR_INV (f))
      bdd_handle_aux (BDD_COMPL (PTR (f)), BDD_NO_OP, 1);
    else
      bdd_handle_aux (PTR (f), BDD_NO_OP, 1);

    USE (handle_end_expr) ();
  }
}

/*
   Handles the nodes of the vector, using the subexpressions and other root
   references.
*/
static void handle_root_vec (BDDPTR *F, int vec_size)
{
  int k;

  for (k = 0; k < vec_size; k++) {
    USE (handle_begin_root) (k);

    if ((BDD_VOID_P (F[k])) || BDD_TERM_P (F[k]))
      USE (handle_const) (F[k]);
    else {
      /* Handle the function of the root node. If the index != k then
	 another pointer in the vector uses the same node, so only
	 the index of that node is printed.
      */
      if (   !BDD_SUBEXPR_FLAG (F[k])
	  || k == BDD_SUBEXPR_INDEX (F[k])
	  || BDD_LIT_P (F[k])) {
        bdd_handle_aux (F[k], BDD_NO_OP, 1);

	/* Allow already `printed' top-level expressions to be used in
	   succeeding ones as long as this wasn't a real subexpression
	   already and of course not a simple literal.
	*/
	if (!BDD_SUBEXPR_FLAG (F[k]) && !BDD_LIT_P (F[k])) {
	  BDD_SUBEXPR_FLAG (F[k]) = 1;
	  BDD_ROOT_FLAG (F[k]) = 1;
	  BDD_SUBEXPR_INDEX (F[k]) = k;
	  BDD_SUBEXPR_INV (F[k]) = BDD_NEG_P (F[k]);
	}
      }
      else {
	int negated = BDD_NEG_P (F[k]) != BDD_SUBEXPR_INV (F[k]);

	if (BDD_ROOT_FLAG (F[k]))
	  USE (handle_root_index) (BDD_SUBEXPR_INDEX (F[k]), negated);
	else
	  USE (handle_sub_index) (BDD_SUBEXPR_INDEX (F[k]), negated);
      }
    }
    USE (handle_end_root) ();
  }
}

/*
   This action counts the number of edges that point to a node. In case of
   exclusive ors, the double edges are counted only once.
*/
static void count_inedges_action (BDDPTR v)
{
  BDDPTR T = BDD_THEN (v);
  BDDPTR E = BDD_ELSE (v);

  BDD_AUX1_L (v) = 0;  /* erase aux field first */

  if (!BDD_TERM_P (v)) {
    BDD_INCR_INCOUNT (T);
    if (!current_interface->use_xors || PTR (T) != PTR (E) || BDD_LIT_P (v))
      BDD_INCR_INCOUNT (E);
  }
}

/* Used for naming the subexpressions. */
static void name_action (BDDPTR v)
{
  if (!BDD_TERM_P (v) && !BDD_ROOT_FLAG (v))
    BDD_SUBEXPR_INDEX (v) = BDD_SUBEXPR_FLAG (v) ? count++ : 0;
}

/*
   The positive size = the number of positive literals that will be printed,
   the negative size = the number of negative literals that will be printed.

   If there is an XOR, use the same sizes as the child node+1 (upper bound).

   When output inverters are NOT used, negative literals should have
   negative size 1.
*/
static void calc_nr_of_literals_action (BDDPTR v)
{
  BDDPTR T, E;
  int size;

  /* Information associated with node v is to be interpreted as if
     the edge v is positive, i.e., not a complemented edge.
  */

  if (BDD_TERM_P (v)) {
    /* No literals in a constant: */
    BDD_POS_SIZE (v) = 0;
    BDD_NEG_SIZE (v) = 0;
    return;
  }

  if (BDD_LIT_P (v)) {
    /* Node is a literal. */
    if (!bdd_use_neg_edges && BDD_NEGLIT_P (v)) {
      /* Count as a negative literal: */
      BDD_POS_SIZE (v) = 0;
      BDD_NEG_SIZE (v) = 1;
    }
    else {
      /* Count as a positive literal: */
      BDD_POS_SIZE (v) = 1;
      BDD_NEG_SIZE (v) = 0;
    }
    return;
  }

  T = BDD_THEN (v);
  E = BDD_ELSE (v);

  /* In case of exclusive ors, only the total size is exact, and not the
     number of positive and negative literals.
  */
  if (current_interface->use_xors && PTR (T) == PTR (E)) {
    BDD_POS_SIZE (v) = BDD_POS_SIZE (T) +
      (BDD_POS_SIZE (T) < MAXFACTORSIZE) ? 1 : 0;
    BDD_NEG_SIZE (v) = BDD_NEG_SIZE (T);
    return;
  }
  /* Calculate nr of positive literals, take special cases into account:
     v Expr + v' 1 --> Expr + v' --> don't increase POS_SIZE by 1
     v 0 + v' Expr --> v' Expr   --> don't increase POS_SIZE by 1
  */
  size =  (BDD_NEG_P (T) ? BDD_NEG_SIZE (T) : BDD_POS_SIZE (T))
        + (BDD_NEG_P (E) ? BDD_NEG_SIZE (E) : BDD_POS_SIZE (E))
	+ ((BDD_1_P (E) || BDD_0_P (T)) ? 0 : 1);
  /* If the size is bigger than MAXFACTORSIZE, then the size
     is limited to that value.
  */
  BDD_POS_SIZE (v) = size < MAXFACTORSIZE ? size : MAXFACTORSIZE;

  /* Calculate nr of negative literals, take special cases into account:
     v Expr + v' 0 --> v Expr    --> don't increase NEG_SIZE by 1
     v 1 + v' Expr --> v + Expr  --> don't increase NEG_SIZE by 1
  */
  size =  (BDD_NEG_P (T) ? BDD_POS_SIZE (T) : BDD_NEG_SIZE (T))
        + (BDD_NEG_P (E) ? BDD_POS_SIZE (E) : BDD_NEG_SIZE (E))
        + ((BDD_0_P (E) || BDD_1_P (T)) ? 0 : 1);
  BDD_NEG_SIZE (v) = size < MAXFACTORSIZE ? size : MAXFACTORSIZE;
}

/*
   This action is used for marking subexpressions using the factor matrix;
   a root node is always marked as a subexpression.
*/
static void mark_candidate_action (BDDPTR v)
{
  register int size;
  register int in;
  register int i;

  calc_nr_of_literals_action (v);

  size = BDD_NEG_SIZE (v) + BDD_POS_SIZE (v);
  in   = BDD_INEDGE_COUNT (v);
  BDD_SUBEXPR_FLAG (v) = BDD_ROOT_FLAG (v);

  for (i = 0; i <= current_interface->maxI; i++)
    if (   size >= current_interface->A[i].limitsize
	&&   in >= current_interface->A[i].in) {
      BDD_SUBEXPR_FLAG (v) = 1;
      break;
    }

  if (BDD_SUBEXPR_FLAG (v)) {
    if (!BDD_ROOT_FLAG (v) && BDD_NEG_SIZE (v) > BDD_POS_SIZE (v))
      BDD_SUBEXPR_INV (v) = bdd_use_neg_edges;

    if (BDD_SUBEXPR_INV (v)) {
      /* Subexpression name will be used complemented. */
      BDD_POS_SIZE (v) = 0;
      BDD_NEG_SIZE (v) = 1;
    }
    else {
      /* Subexpression name will be used positively. */
      BDD_POS_SIZE (v) = 1;
      BDD_NEG_SIZE (v) = 0;
    }
  }
}

/*
   This function factors the BDDs in vector F, and prints the complete set
   of expressions with the current factor interface.
*/
void bdd_factor_vec (BDDPTR *F, int vec_size)
{
  int k;

  /* The algorithms do not work when input inverters are used: */
  if (bdd_use_inv_edges) {
    fprintf (stderr, "[bdd_factor]: Cannot handle inverted inputs.\n");
    return;
  }

  /* Count nr. of references (edges) to each node in F,
     top-level nodes are excluded.
  */
  bdd_traverse_vec_post (F, vec_size, count_inedges_action);
  /* Marks are 1 */

  if (current_interface->style)
    /* Define the root nodes. For each the root node flag is set.
       Since they may be shared, we treat them in reverse order such that
       the first reference (counted from 0) determines whether it will be
       complemented or not. 
    */
    for (k = vec_size - 1; k >= 0; k--)
      if (!BDD_VOID_P (F[k])) {
	BDD_ROOT_FLAG (F[k]) = 1;
	BDD_SUBEXPR_INV (F[k]) = BDD_NEG_P (F[k]);
      }

  /* Mark nodes for which a subexpression will be created.
     First calculates the number of positive and negative literals per node.
  */
  bdd_traverse_vec_post (F, vec_size, mark_candidate_action);
  /* Marks are 0 */

  if (current_interface->style)
    /* Numbering of the roots:
       the index of the first appearance of every root node in the vector
       is stored in BDD_SUBEXPR_INDEX of that node.

       NOTE: from now on, the second struct of the aux field is used, so
       parts of the information of the previous struct are lost!
    */
    for (k = vec_size - 1; k >= 0; k--)
      if (!BDD_VOID_P (F[k]))
	/* Use vector index as index number: */
	BDD_SUBEXPR_INDEX (F[k]) = k;

  /* Numbering of the subexpressions:
     every subexpression is given a unique number, starting from zero.
  */
  count = 0;

  if (current_interface->style) {
    bdd_traverse_vec_pre (F, vec_size, name_action);
    /* Marks are 1 */

    /* Handle the root nodes of the vector */
    USE (handle_root_exprs) (vec_size);
    handle_root_vec (F, vec_size);

    /* Handle the subexpressions of the vector */
    USE (handle_sub_exprs) (count);
    bdd_traverse_vec_pre (F, vec_size, print_action);
    /* Marks are 0 */
  }
  else {
    bdd_traverse_vec_post (F, vec_size, name_action);
    /* Marks are 1 */

    /* Handle the subexpressions of the vector */
    USE (handle_sub_exprs) (count);
    bdd_traverse_vec_post (F, vec_size, print_action);
    /* Marks are 0 */

    /* Handle the root nodes of the vector */
    USE (handle_root_exprs) (vec_size);
    handle_root_vec (F, vec_size);
  }

  /* Cleanup: */
  bdd_traverse_vec_post (F, vec_size, bdd_reinit_aux1_action);
  /* Marks are 1 */
  bdd_traverse_vec_post (F, vec_size, bdd_null_action);
  /* Marks are 0 */
}

void bdd_factor (BDDPTR f)
{
  bdd_factor_vec (&f, 1);
}

bdd_factor_interface *bdd_get_factor_interface (void)
{
  return current_interface;
}

void bdd_set_factor_interface (bdd_factor_interface *new_interface)
{
  if (new_interface)
    current_interface = new_interface;
  else
    current_interface = &bdd_default_factor_interface;
}
