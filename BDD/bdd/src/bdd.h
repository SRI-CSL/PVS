/*
 DOCUMENTATION INFORMATION				         module: BDD
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system	   : Apollo DN3000, HP 9000/S500, IBM RS/6000
 file	   : bdd.h
 unit-title: 
 ref.	   : Efficient Implementation of a BDD Package, Karl S. Brace. DAC'90
 author(s) : Copyright (c) 1990-1998 G.L.J.M. Janssen
 date	   : 13-JAN-1998
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

#ifndef BDD_H
#define BDD_H

/* ------------------------------------------------------------------------ */
/* INCLUDES                                                                 */
/* ------------------------------------------------------------------------ */

#include "general.h"
#ifdef BDD_LIGHT
#include "bdd_alloc.h"
#else
#include "alloc.h"
#include "list.h"
#include "bdd_list.h"
#include "double.h"
#endif
#include "bdd_extern.h"

/* IMPORTANT NOTICE!!!

   This package freely uses the two least significant bits of a pointer
   value, which is assumed to be a 32-bit or 64-bit quantity.
   It therefore is assumed that BDD structs are all aligned on a word
   boundary (for addresses both bits are always 0). Since the package
   does it own BDD nodes allocation in blocks, this is always assured.
   If on certain machines this does not seem to be valid, the mal-use of
   the two least significant bits can be turned off by setting the global
   variables `use_neg_edges' and `use_inv_edges' both to 0.

   It is also assumed that the C type `int' is a 32-bit quantity.
   Perhaps on some machines all `int's should be replaced by `long int'.

   Bit 0 indicates the least significant bits of a word.
   To indicate a complemented edge, bit 0 of the BDDPTR is used.
   To indicate an inverted input variable, bit 1 of the BDDPTR is used.
*/

/* ------------------------------------------------------------------------ */
/* DEFINES                                                                  */
/* ------------------------------------------------------------------------ */

/* Absolute program limitiations:
   - at most BDD_MAXVARS input variables
*/

/* BDD_MAXVARS+1:  BDD_MAXREFCOUNT: */
/* 2^14 =   16384, 2^16-1 = 65535 */
/* 2^16 =   65536, 2^14-1 = 16383 */
/* 2^20 = 1048576, 2^10-1 =  1023 */
/* 2^22 = 4194304, 2^ 8-1 =   255 */
/* Number of bits for BDD node's variable identification: */
#define BDD_NR_ID_BITS		16
/* Number of bits for BDD node's reference count: */
#define BDD_NR_RC_BITS		(32-BDD_NR_ID_BITS-2)
/* Maximum ref count value; when reached, node gets frozen. */
#define BDD_MAXREFCOUNT		(((unsigned int) 1 << BDD_NR_RC_BITS) - 1)
/* BDD_TERMID is reserved for terminal nodes. */
#define BDD_TERMID	  ((int)(((unsigned int) 1 << BDD_NR_ID_BITS) - 1))
/* BDD_MAXVARID is maximum id number for a user variable. */
#define BDD_MAXVARID		(BDD_TERMID-1)
/* BDD_MAXVARS is the maximum number of user variables. */
#define BDD_MAXVARS		BDD_TERMID
/* Variables are ranked starting from 0; this is the max. value. */
#define BDD_MAXRANK		BDD_MAXVARID

/* Attributed edges masks: output and input inverters: */
#define BDD_O_INV_MASK		0x1L
#define BDD_I_INV_MASK		0x2L
#define BDD_INV_MASK		(BDD_O_INV_MASK | BDD_I_INV_MASK)

/* Look at edge as a set of bits: */
#define BITS(F)			((long) F)

/* Look at edge as a real pointer: */
#define PTR(F)			((BDDPTR) (BITS (F) & ~BDD_INV_MASK))

/* Access to various fields of BDD node: */
/* Not guarded against BDD_VOID arg! */
#define BDD_VARID(F)		(PTR (F)->varid)
#define BDD_THEN(F)		(PTR (F)->then_link)
#define BDD_ELSE(F)		(PTR (F)->else_link)
#define BDD_NEXT(F)		(    (F)->next)
#define BDD_REFCOUNT(F)		(PTR (F)->refcount)
#define BDD_AUX(F)		(PTR (F)->aux)
#define BDD_AUX_D(F)		(PTR (F)->aux.d)
#define BDD_AUX_DBL(F)		(PTR (F)->aux.dbl)
#define BDD_AUX1(F)		(PTR (F)->aux.aux.aux1)
#define BDD_AUX1_L(F)		(PTR (F)->aux.aux.aux1.l)
#define BDD_INFO(F)		BDD_AUX1_L (F)
#define BDD_AUX1_BDD(F)		(PTR (F)->aux.aux.aux1.bdd)
#define BDD_AUX1_LIST(F)	(PTR (F)->aux.aux.aux1.list)
#define BDD_AUX1_STR(F)		(PTR (F)->aux.aux.aux1.str)
#define BDD_AUX1_PTR(F)		(PTR (F)->aux.aux.aux1.ptr)
#define BDD_AUX1_FLT(F)		(PTR (F)->aux.aux.aux1.flt)
#define BDD_AUX2(F)		(PTR (F)->aux.aux.aux2)
#define BDD_AUX2_L(F)		(PTR (F)->aux.aux.aux2.l)
#define BDD_AUX2_BDD(F)		(PTR (F)->aux.aux.aux2.bdd)
#define BDD_AUX2_LIST(F)	(PTR (F)->aux.aux.aux2.list)
#define BDD_AUX2_STR(F)		(PTR (F)->aux.aux.aux2.str)
#define BDD_AUX2_PTR(F)		(PTR (F)->aux.aux.aux2.ptr)
#define BDD_AUX2_FLT(F)		(PTR (F)->aux.aux.aux2.flt)
/* Returns pointer to start of user_data field in bdd node. */
#define BDD_USER_DATA_PTR(F)	(((char *) PTR (F)) + sizeof (struct bdd))
/* Flagging BDD nodes for pointer-reversal technique: */
#define BDD_FLAG(F)		(PTR (F)->flag)
#define BDD_FLAGGED(F,G)	(PTR (F)->flag == PTR (G)->flag)
#define BDD_NOT_FLAGGED(F,G)	(PTR (F)->flag != PTR (G)->flag)
#define BDD_TOGGLE_FLAG(F)	do {PTR (F)->flag ^= 1;} while (0)
/* Marking BDD nodes to assist in efficient DAG traversal: */
#define BDD_MARK(F)		(PTR (F)->mark)
#define BDD_MARKED(F,G)		(PTR (F)->mark == PTR (G)->mark)
#define BDD_NOT_MARKED(F,G)	(PTR (F)->mark != PTR (G)->mark)
#define BDD_TOGGLE_MARK(F)	do {PTR (F)->mark ^= 1;} while (0)
#define BDD_UNMARK(F)		do {PTR (F)->mark = 0;} while (0)

/* Saturating refcount operations: */
#define BDD_FROZEN_P(F)		(BDD_REFCOUNT (F) == BDD_MAXREFCOUNT)
#define BDD_NOT_FROZEN_P(F)	(!BDD_FROZEN_P (F))
#define BDD_FREEZE(F)		do { \
				  if (BDD_REFCOUNT (F) < BDD_MAXREFCOUNT) { \
                                    if (BDD_DEAD_P (F)) bdd_nr_dead_nodes--; \
				    bdd_nr_frozen_nodes++; \
			            BDD_REFCOUNT (F) = BDD_MAXREFCOUNT; \
			          } \
			        } while (0)

/* Guarded against BDD_VOID arg! */
#define BDD_INCR_REF(F)		do { \
				  if (   !BDD_VOID_P (F) \
				      && BDD_REFCOUNT (F) < BDD_MAXREFCOUNT) {\
                            	    if (BDD_DEAD_P (F)) bdd_nr_dead_nodes--; \
			    	    BDD_REFCOUNT (F)++; \
			    	    if (BDD_FROZEN_P (F)) \
				      bdd_nr_frozen_nodes++; \
			  	  } \
				} while (0)
/* Alias kept for compatibility; better use bdd_assign(). */
#define BDD_GC_PROTECT(F)	BDD_INCR_REF (F)

/* Not guarded against BDD_VOID arg! */
/* pre: refcount > 0 */
#define BDD_DECR_REF(F)		do { \
				  if (BDD_REFCOUNT (F) < BDD_MAXREFCOUNT) { \
			    	    BDD_REFCOUNT (F)--; \
  			    	    if (BDD_DEAD_P (F)) bdd_nr_dead_nodes++; \
			  	  } \
				} while (0)

#define BDD_DEAD_P(F)		(BDD_REFCOUNT (F) == 0)

/* Test whether edge v is postive or negative: */
#define BDD_POS_P(F)		(!(BITS (F) & BDD_O_INV_MASK))
#define BDD_NEG_P(F)		!BDD_POS_P (F)

#define BDD_O_INV_EDGE_P(F)	BDD_NEG_P (F)

/* Test whether edge v refers to inverted input node: */
#define BDD_I_INV_EDGE_P(F)	(!!(BITS (F) & BDD_I_INV_MASK))

/* Setting and clearing of edge attribute bits: */
#define BDD_O_OFF(F)	((BDDPTR) (BITS (F) & ~BDD_O_INV_MASK))
#define BDD_O_SET_U(F)  ((BDDPTR) (BITS (F) | BDD_O_INV_MASK))
/* Special case for BDD_X and user terminals. Never have O_INV bit set! */
#define BDD_O_SET(F)	((BDD_TERM_P (F) && !BDD_BOOL_P (F)) ? (F) \
			 : BDD_O_SET_U (F))

#define BDD_I_OFF(F)	((BDDPTR) (BITS (F) & ~BDD_I_INV_MASK))
#define BDD_I_SET_U(F)	((BDDPTR) (BITS (F) | BDD_I_INV_MASK))
/* Special case for terminals. Never have I_INV bit set! */
#define BDD_I_SET(F)	(BDD_TERM_P (F) ? (F) : BDD_I_SET_U (F))

/* Complement edge taking special cases into account:
   (Note: only meaningful when negated edges are used; otherwise use the
          more general bdd_not function!
*/
#define BDD_COMPL(F)	(BDD_NEG_P (F) ? BDD_O_OFF (F) : BDD_O_SET (F))

/* Invert edge taking special cases into account: */
#define BDD_INV(F)	(BDD_I_INV_EDGE_P (F) ? BDD_I_OFF (F) : BDD_I_SET (F))

/* Predicates for terminal BDD nodes: */
#define BDD_VOID_P(F)	BDD_EQUAL_P (F, BDD_VOID)

#define BDD_0_P(F)	BDD_EQUAL_P (F, BDD_0)

#define BDD_1_P(F)	BDD_EQUAL_P (F, BDD_1)

#define BDD_X_P(F)	BDD_EQUAL_P (F, BDD_X)

/* Test on Boolean truth value: */
#define BDD_BOOL_P(F)	(BDD_0_P (F) || BDD_1_P (F))

/* This tests for a BDD constant not just any terminal node: */
#define BDD_CONST_P(F)	(BDD_BOOL_P (F) || BDD_X_P (F))

/* This tests for any terminal node, i.e, the predefined BDD constants
   BDD_0, BDD_1, and BDD_X, but also any user-introduced terminal.
   User-introduced terminals cannot have any special modification bits
   set on their pointers!
*/
#define BDD_TERM_P(F)	(BDD_VARID (F) == BDD_TERMID)

/* Test on regular, i.e., internal, BDD node: */
#define BDD_INTERN_P(F)	(BDD_VARID (F) < BDD_TERMID)

/* Test for user-introduced terminal node: */
#define BDD_USER_TERM_P(F)	(BDD_TERM_P (F) && !BDD_CONST_P (F))

/* Get data of user-introduced terminal BDD node: */
#define BDD_USER_TERM_DATA(F)	BDD_THEN (F)

/* Interpret (any) terminal node as truth-value: */
#define BDD_FALSE_P(f)	 BDD_0_P (f)
#define BDD_TRUE_P(f)	!BDD_FALSE_P (f)

/* Convert C condition value to BDD constant: */
#define BDD_BOOL(cond)	((cond) ? BDD_1 : BDD_0)

/* BDD node represents a literal? */
#define BDD_LIT_P(F)	(   BDD_INTERN_P (F) \
			 && (   (   BDD_1_P (BDD_THEN (F)) \
			         && BDD_0_P (BDD_ELSE (F))) \
			     || (   BDD_0_P (BDD_THEN (F)) \
				 && BDD_1_P (BDD_ELSE (F)))))

#define BDD_POSLIT_P(F)	(   BDD_1_P (BDD_COFACTOR_POS (F)) \
			 && BDD_0_P (BDD_COFACTOR_NEG (F)))

#define BDD_NEGLIT_P(F)	(   BDD_0_P (BDD_COFACTOR_POS (F)) \
			 && BDD_1_P (BDD_COFACTOR_NEG (F)))

/* Equality on functions: */
#define BDD_EQUAL_P(F1, F2)	((F1) == (F2))

/* Are functions each other's complement:
   (Only correct when bdd_use_neg_edges == 1)
*/
#define BDD_COMPL_P(F1, F2)	((BITS (F1) ^ BITS (F2)) == BDD_O_INV_MASK)

/* The rank value of variable id; lowest rank is 0. */
#define BDD_VAR_RANK(v)		(((v) == BDD_TERMID) \
				 ? BDD_TERMID \
				 : unique_table.ranks[v])

#define BDD_RANK(F)		BDD_VAR_RANK (BDD_VARID (F))

#define BDD_NR_VARIDS_RESERVED	(unique_table.count)

/* Logical Operators: */
#define     BDD_NOT(F)	 bdd_ite(F, BDD_0, BDD_1)	/* ~F */

#define    BDD_ZERO(F,G) bdd_0()			/* 0 */
#define     BDD_AND(F,G) bdd_ite(F,G,BDD_0)		/* F ^ G */
#define BDD_GREATER(F,G) bdd_greater(F,G)		/* F > G */
#define   BDD_PROJ1(F,G) bdd_assign(F)			/* F */
#define    BDD_LESS(F,G) bdd_ite(F,BDD_0,G)		/* F < G */
#define   BDD_PROJ2(F,G) bdd_assign(G)			/* G */
#define     BDD_XOR(F,G) bdd_xor(F,G)			/* F exor G */
#define      BDD_OR(F,G) bdd_ite(F,BDD_1,G)		/* F V G */
#define     BDD_NOR(F,G) bdd_nor(F,G)			/* ~(F V G) */
#define   BDD_EQUIV(F,G) bdd_equiv(F,G)			/* ~(F exor G) */
#define    BDD_XNOR(F,G) bdd_xnor(F,G)			/* F xnor G */
#define    BDD_NOT2(F,G) BDD_NOT(G)			/* ~G */
#define BDD_IMPLIED(F,G) bdd_implied(F,G)		/* F <- G */
#define    BDD_NOT1(F,G) BDD_NOT(F)			/* ~F */
#define BDD_IMPLIES(F,G) bdd_ite(F,G,BDD_1)		/* F -> G */
#define    BDD_NAND(F,G) bdd_nand(F,G)			/* ~(F ^ G) */
#define     BDD_ONE(F,G) bdd_1()			/* 1 */

#define BDD_IMPLIES_TAUT(F,G) BDD_1_P (bdd_ite_const(F,G,BDD_1))

/* Does F cover (= contain) G, i.e. (G -> F) = 1 ? Or G <= F = 1. */
/* Test on BDD_EQUAL not really necessary: just for speed. */
#define BDD_COVERS(F,G)		(   BDD_EQUAL_P (F, G) \
                        	 || BDD_IMPLIES_TAUT (G, F))
#define BDD_CONTAINS(F,G)	BDD_COVERS (F, G)

/* Cofactors of f with respect to root vertex's (positive) variable.
   Takes complemented-outputs and inverted-inputs into account.
   For BDD_VOID argument or terminal node returns argument.
*/
#define BDD_COFACTOR_POS(F) \
  (BDD_VOID_P (F) || BDD_TERM_P (F)) \
    ? (F) \
    : (BDD_I_INV_EDGE_P (F) \
       ? (BDD_NEG_P (F) \
	  ? BDD_COMPL (BDD_ELSE (F)) \
	  : BDD_ELSE (F)) \
       : (BDD_NEG_P (F) \
	  ? BDD_COMPL (BDD_THEN (F)) \
	  : BDD_THEN (F)))

#define BDD_COFACTOR_NEG(F) \
  (BDD_VOID_P (F) || BDD_TERM_P (F)) \
    ? (F) \
    : (BDD_I_INV_EDGE_P (F) \
       ? (BDD_NEG_P (F) \
	  ? BDD_COMPL (BDD_THEN (F)) \
	  : BDD_THEN (F)) \
       : (BDD_NEG_P (F) \
	  ? BDD_COMPL (BDD_ELSE (F)) \
	  : BDD_ELSE (F)))

/* ------------------------------------------------------------------------ */
/* LOCAL TYPE DEFINITIONS FOR BDD_FACTOR.C                                  */
/* ------------------------------------------------------------------------ */

/* Nr. bits allocated for inedge counter per BDD node.
   Counter saturates at MAXINEDGECOUNT value.
*/
#define NR_INEDGE_COUNT_BITS     5

/* Nr. bits allocated for left/right sub sizes per node.
   Counter saturates at MAXFACTORSIZE value.
*/
#define NR_SIZE_BITS             ((32-3-NR_INEDGE_COUNT_BITS) >> 1)

/* Nr. bits allocated for sub expression id. */
#define NR_INDEX_BITS            (32-3)

/* These struct precisely overlay the 32-bit AUX1 field in a BDD node. */
typedef struct {
  unsigned int subexpr_inv   :  1;
  unsigned int root_flag     :  1;
  unsigned int subexpr_flag  :  1;
  unsigned int inedge_cnt    :  NR_INEDGE_COUNT_BITS;
  unsigned int pos_size      :  NR_SIZE_BITS;
  unsigned int neg_size      :  NR_SIZE_BITS;
#if BITS64 == 1
  unsigned int padding;
#endif
} factor1_aux;

typedef struct {
  unsigned int subexpr_inv   :  1;
  unsigned int root_flag     :  1;
  unsigned int subexpr_flag  :  1;
  unsigned int subexpr_index :  NR_INDEX_BITS;
#if BITS64 == 1
  unsigned int padding;
#endif
} factor2_aux;

/* ------------------------------------------------------------------------ */
/* LOCAL TYPE DEFINITIONS FOR BDD_FNS.C                                     */
/* ------------------------------------------------------------------------ */

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
#if BITS64 == 1
  unsigned long padding;
#endif
} path_rec;

/* ------------------------------------------------------------------------ */
/* TYPE DEFINITIONS                                                         */
/* ------------------------------------------------------------------------ */

/* Useful for indicating type of aux field; could e.g. use aux1 to
   keep type of value in aux2.
*/
typedef enum {
  BDD_AUX_LONG,
  BDD_AUX_BDD,
  BDD_AUX_LIST,
  BDD_AUX_STR,
  BDD_AUX_PTR,
  BDD_AUX_FLT
} BDD_AUX_TYPES;

typedef unsigned char BYTE;

typedef union {
  BDD_AUX_TYPES type;
  long l;
  BDDPTR bdd;
#ifdef BDD_LIGHT
  void *list;
#else
  BDD_LIST list;
#endif
  char *str;
  void *ptr;
  float flt;
  factor1_aux factor1;
  factor2_aux factor2;
} bdd_aux_field;

typedef union {
  struct {
    bdd_aux_field aux1;
    bdd_aux_field aux2;
  } aux;
  path_rec path;
#ifdef BDD_LIGHT
  double d;
#else
  Double d;
#endif
  double dbl;
} bdd_aux_union;

struct bdd {
  unsigned varid:BDD_NR_ID_BITS;/* id of variable, Xi, counted from 0 */
  unsigned flag :1;		/* Left/Right flag used in BDD traversal */
  unsigned mark :1;		/* mark bit used in BDD traversal */
  unsigned refcount:BDD_NR_RC_BITS; /* saturating reference count */
#if BITS64 == 1
  unsigned padding;
#endif
  BDDPTR then_link;		/* child for variable = 1 */
  BDDPTR else_link;		/* child for variable = 0 */
  BDDPTR next;			/* chaining in unique table */
  bdd_aux_union aux;		/* general purpose, initialized to 0! */
/* Accessory user specified fields may be allocated here.
   Size of this in bytes is kept in bdd_sizeof_user_data.
*/
};

/* A hash table for BDD nodes of the same variable. */
typedef struct hash_table *V_HASHTAB;

/* The unique table, a hash table with chaining: */
typedef struct unique_table UNIQUE_TABLE;

/* The record for info on a group of variables (ranks). */
typedef struct {
  unsigned int orderable :  1;
  unsigned int last_rank : 31;
} GROUP_REC;

struct unique_table {
  int nr_entries;		/* total entries in V_HASHTABs */
  int size;			/* # ranks anticipated */
  int count;			/* 0 <= # hash_tables in use <= size */
  int nr_items;			/* total # BDD nodes in table */
  int nr_groups;		/* # of groups of ranks */
  GROUP_REC *groups;		/* the groups */
  int *ranks;			/* rank of varid */
  V_HASHTAB *space;		/* the hash tables per rank */
  V_HASHTAB terms;		/* collects user terminal nodes */
};

/* ABOUT THE MEMORY REQUIREMENTS

   Layout of BDDPTR (pointer to BDD record):

          31 30 29 28 27 26 ......................2 | 1 | 0 
         +------------------------------------------+---+---+
         |                                          | I | O |
         +------------------------------------------+---+---+

   Layout of BDD record:

          31 30 29 28 27 26 ... 16 |15 |14 |13 6 5 ... 2 1 0
         +-------------------------+---+---+----------------+
         |       variable id       | E | M |    refcount    |
         +-------------------------+---+---+----------------+
         |                 then_link                        |
         +--------------------------------------------------+
         |                 else_link                        |
         +--------------------------------------------------+
         |                   next                           |
         +--------------------------------------------------+
         |          aux1 (general purpose)                  |
         +--------------------------------------------------+
         |          aux2 (general purpose)                  |
         +--------------------------------------------------+
         .           optional user data                     .
         .                                                  .
         +..................................................+

	 id   : indirect reference to rank number of a variable,
	 E    : pointer-reversal traversal bit, when set indicates that
	        when back at this node must still examine else edge,
	 M    : mark bit in traversal, when set means that this node
	 	has been visited.

   There are at most twice as many edges as nodes in a BDD graph.
   The edges, however, are not explicitly represented; they are the
   then_link and else_link pointer fields in each node.
   Given that the sizeof (BDD) is 24 bytes, a graph with n nodes
   occupies at most 24n bytes of memory. The overhead caused by the
   unique_table can be calculated as follows: on average there are 4
   (= LOAD_FACTOR) nodes per entry. The entry itself costs 4 bytes,
   that is 1 byte extra for every node. In total for n nodes we now have
   24n + n = 25n bytes. But this is in an ideal situation where the
   table is completely full. The computed_table has the same size has
   the unique_table.

   Another way of implementing BDD's would be to keep the index for
   a node not in the node record itself but store it in the edge.
   This requires even more pointer bits to be available than the two
   we are already using as flag bits.

   A NOTE ABOUT BDD INDICES, ID's and VARIABLE ORDER:

   The BDD struct has a field called varid. This is meant to uniquely
   identify a variable (propositional variable / primary input in the
   logic circuit). Next to this there exists the notion of variable
   ordering. This package offers to keep those things separate.
   It allows for dynamically introducing variables in any order.
   The macro BDD_RANK (F) will return the rank order number associated
   with the variable in the BDD node F. This variable itself is identified
   by the varid field in f accessible through the macro BDD_VARID (F).
   If an application does not wish to use this feature and thus relies
   on the fact that it has a static variable ordering before hand,
   then only the routine bdd_create_var should be used to introduce
   new variables. It is then guaranteed that BDD_RANK and BDD_VARID
   behave identically: a variable's identity is its rank number,
   in fact, BDD_VAR_RANK (id) == id.
   On the other hand, when an application wants to introduce variables
   with non-consecutive rank numbers, the routines: bdd_create_var_first,
   bdd_create_var_after, and bdd_create_var_last should be used.

   Summarizing:
   -----------
   There are two ways to handle variable ordering:

   1) The identification number of a variable is used as its rank order
      number. Then variables may be created in any order, but once created
      it is no longer possible to create arbitrary many new variables
      before or after any existing variable. Once an id number is used
      that rank number is used and cannot be changed.
      Only the routine "bdd_create_var" must be used.

   2) Variables are identified by consecutive numbers starting from 0.
      Each call to either "bdd_create_var_first", "bdd_create_var_after",
      and "bdd_create_var_last" will create a NEW variable BDD, and
      its id number will be 1 higher than the last created variable.
      Given a id number of a variable it is always possible to get
      its associated BDD by calling "bdd_create_var (id)" which checks
      that indeed such a variable was already created before..
*/

/* ------------------------------------------------------------------------ */
/* VARIABLES                                                                */
/* ------------------------------------------------------------------------ */
//static char SccsId_BDD_H[] = "%Z%%Y%/%M% %I% %G%";

extern UNIQUE_TABLE unique_table;

extern int bdd_size_limit;
extern int bdd_dyna_monitor;
extern float bdd_dyna_threshold_factor;

/* ------------------------------------------------------------------------ */
/* FUNCTION PROTOTYPES                                                      */
/* ------------------------------------------------------------------------ */

extern   void bdd_free_aux1_action		(BDDPTR v);
extern   void bdd_free_aux1_and_aux2_action	(BDDPTR v);
extern   void bdd_reinit_aux1_action		(BDDPTR v);
extern   void bdd_reinit_aux1_and_aux2_action	(BDDPTR v);

#endif /* BDD_H */
