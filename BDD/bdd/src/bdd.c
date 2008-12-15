/*
 DOCUMENTATION INFORMATION				           module: BDD
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system	   : HP 9000/S750
 file	   : bdd.c
 unit-title: 
 ref.	   : Efficient Implementation of a BDD Package, Karl S. Brace. DAC'90
             Dynamic variable ordering for ordered binary decision diagrams,
             Richard Rudell, IWLS '93 Workshop Notes
 author(s) : Copyright (c) 1990-1998 G.L.J.M. Janssen
 credits   : Thanks to Koen van Eijk and Arjen Mets.
 date	   : 15-APR-1998
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

#ifdef __MWERKS__

/* this is not a C++ file, but CodeWarrior won't recognize this... */

#pragma cplusplus off
#endif

/* ------------------------------------------------------------------------ */
/* INCLUDES                                                                 */
/* ------------------------------------------------------------------------ */

#include "bdd.h"

/* ------------------------------------------------------------------------ */
/* LOCAL DEFINES                                                            */
/* ------------------------------------------------------------------------ */

#define BDD_MALLOC_BYTES(n, type) \
	( \
	  bdd_bytes_allocated += (n), \
	  bdd_max_bytes_used = max (bdd_bytes_allocated, bdd_max_bytes_used), \
	  MALLOC_BYTES(n, type) \
	)

#define BDD_CALLOC_BYTES(n, type) \
	( \
	  bdd_bytes_allocated += (n), \
	  bdd_max_bytes_used = max (bdd_bytes_allocated, bdd_max_bytes_used), \
	  CALLOC_BYTES(n, type) \
	)

#define BDD_MALLOC_ARRAY(n, type) \
	( \
	  bdd_bytes_allocated += (n) * sizeof (type), \
	  bdd_max_bytes_used = max (bdd_bytes_allocated, bdd_max_bytes_used), \
	  MALLOC_ARRAY(n, type) \
	)

#define BDD_CALLOC_ARRAY(n, type) \
	( \
	  bdd_bytes_allocated += (n) * sizeof (type), \
	  bdd_max_bytes_used = max (bdd_bytes_allocated, bdd_max_bytes_used), \
	  CALLOC_ARRAY(n, type) \
	)

#define BDD_REALLOC_ARRAY(arr, n, type, m) \
	( \
	  bdd_bytes_allocated += ((n) - (m)) * sizeof (type), \
	  bdd_max_bytes_used = max (bdd_bytes_allocated, bdd_max_bytes_used), \
	  MA_REALLOC_ARRAY(arr, n, type, m) \
	)

#define BDD_FREE_BYTES(p, n) \
	do { \
	  bdd_bytes_allocated -= (n); \
	  MA_FREE_BYTES(p, n); \
	} once

#define BDD_FREE_ARRAY(p, n, type) \
	do { \
	  bdd_bytes_allocated -= (n) * sizeof (type); \
	  MA_FREE_ARRAY(p, n, type); \
	} once

/* Note: bdd_nr_dead_nodes not incremented because in context of call
   refcount will always be directly incremented.
*/
#define BDD_MALLOC() \
	( \
	 bdd_free_list ? (bdd_temp = bdd_free_list, \
			  bdd_free_list = BDD_NEXT (bdd_free_list), \
			  bdd_temp) \
			: bdd_alloc () \
	)

#define BDD_CALLOC() \
	( \
	 bdd_nr_dead_nodes++, \
	 bdd_temp = BDD_MALLOC (), *bdd_temp = BDD_NULL, bdd_temp \
	)

#define BDD_FREE(v) \
	do { \
	  BDDPTR _xyz_v = PTR (v); \
	  \
          bdd_nr_dead_nodes--, \
          BDD_NEXT (_xyz_v) = bdd_free_list; \
	  bdd_free_list = _xyz_v; \
	} once

/* Program's default for unique_table load factor: */
#define BDD_DEFAULT_LOAD_FACTOR		4

/* Program's default for computed_table size (2 power): */
#define BDD_DEFAULT_COMPUTED_TABLE_SIZE	pow2(14)

/* Default size of the per rank number hash table (2 power): */
#define BDD_DEFAULT_HASHTAB_SIZE	pow2(7)

/* Default number of unique table entries: */
#define BDD_DEFAULT_NR_RANKS		pow2(8)

#define BDD_VUT_ID(x)			((x)->id)
#define BDD_VUT_LOG2SIZE(x)		((x)->log2size)
#define BDD_VUT_ITEMS(x)		((x)->nr_items)
#define BDD_VUT_ENTRIES(x)		((x)->entries)
#define BDD_VUT_SIZE(x)			pow2(BDD_VUT_LOG2SIZE(x))

#define BDD_VUT_SIZEOF(n)		(sizeof (struct hash_table) \
					 + (pow2(n)-1) * sizeof (BDDPTR))

#define BDD_CT_LOG2SIZE(x)		((x)->log2size)
#define BDD_CT_HITS(x)			((x)->nr_hits)
#define BDD_CT_LOOKUPS(x)		((x)->nr_lookups)
#define BDD_CT_COLLS(x)			((x)->nr_collisions)
#define BDD_CT_ITEMS(x)			((x)->nr_items)
#define BDD_CT_ENTRIES(x)		((x)->entries)
#define BDD_CT_SIZE(x)			pow2(BDD_CT_LOG2SIZE(x))

#define BDD_CT_SIZEOF(n)		\
     (sizeof (struct computed_table) \
      + (pow2(n)-1) * sizeof (COMPUTED_TABLE_ENTRY))

#define BDD_BLK_NEXT(x)			((x)->next)
#define BDD_BLK_SPACE(x)		((x)->space)

/* Number of bytes allocated per block of BDD node storage: */
#define BDD_BLK_SIZEOF			pow2(16)


/* Pointer reversal macros used in traversing a BDD:
   (Using `global' t and b pointers)
*/
#define MOVE_DOWN_THEN(v) \
	do { \
	  t = v; \
	  v = BDD_THEN (v); \
	  BDD_THEN (t) = b; \
	  BDD_FLAG (t) = 0; \
	  b = t; \
	} once

#define MOVE_DOWN_ELSE(v) \
	do { \
	  t = v; \
	  v = BDD_ELSE (v); \
	  BDD_ELSE (t) = b; \
	  BDD_FLAG (t) = 1; \
	  b = t; \
	} once

#define MOVE_UP_AND_RESTART2(v) \
	do { \
	  t = b; \
	  if (BDD_FLAG (t)) { \
	    BDD_FLAG (t) = 0; \
	    b = BDD_ELSE (b); \
	    BDD_ELSE (t) = v; \
	    v = t; \
	  } \
	  else { \
	    b = BDD_THEN (b); \
	    BDD_THEN (t) = v; \
	    v = t; \
	    goto restart2; \
	  } \
	} once

#define CURR_MAXRANK	(unique_table.count - 1)

/* What are groups?
   Groups are a way to partition the user variables. A group is a block
   of the partition. The variables in a group are allocated consecutive
   rank numbers and will during dynamic variable ordering be treated
   as a fixed unit, i.e., groups may be freely interchanged but not
   necessarily the variables within a group (this is user controllable).
*/

/* 0 <= NR_GROUPS <= CURR_MAXRANK */
/* 0 <= group-index < NR_GROUPS */

#define NR_GROUPS		(unique_table.nr_groups)

#define GROUP_ORDERABLE(g)	(unique_table.groups[g].orderable)

#define GROUP_FIRST_RANK(g)	((g) ? GROUP_LAST_RANK((g)-1U)+1U : 0U)

#define GROUP_LAST_RANK(g)	(unique_table.groups[g].last_rank)

#define GROUP_NR_RANKS(g)	(GROUP_LAST_RANK(g)-GROUP_FIRST_RANK(g)+1U)

/* bdd_apply uses computed_table but entry->F will be some function
   pointer of type BDDPTR (*f)(BDDPTR,BDDPTR).
   We need to distinguish this from a regular BDDPTR, therefore we set
   the most-significant bit on those function pointers. There is no harm
   when the pointer value has already its MSB set: the value is never
   used except for comparison with another function pointer.
   If we cannot guarantee that BDD pointers have a 0 MSB then our trick
   falls apart. This is tested in bdd_alloc().
*/
#define BDD_F_MASK	0x80000000
#define BDD_F_SET(F)	((BDDPTR) (BITS (F) | BDD_F_MASK))
#define BDD_F_SET_SAVE(F)	\
	(bdd_ok_to_use_MSB ? BDD_F_SET(F) \
                           : (fputs( \
"[bdd]: Not allowed to use MSB of BDD pointer.\n", stderr), \
			      exit(1), BDD_VOID))
#define BDD_FUNC_P(F)	(bdd_ok_to_use_MSB && (BITS (F) & BDD_F_MASK))

/* ------------------------------------------------------------------------ */
/* LOCAL TYPE DEFINITIONS                                                   */
/* ------------------------------------------------------------------------ */

/* List of blocks of BDD nodes: */
typedef struct block *BLKPTR;

struct block {
  BLKPTR next;
  struct bdd space[1];
};

struct hash_table {
  Nat id : BDD_NR_ID_BITS;	/* varid associated with this table */
  Nat log2size : 32-BDD_NR_ID_BITS; /* 2log of table size */
  int nr_items;			/* # BDD nodes in table */
  BDDPTR entries[1];		/* the chains */
};

/* The computed table, a closed hash table with overwrite (cache): */
typedef struct computed_table_entry COMPUTED_TABLE_ENTRY;

/* Note: occurrence in the computed table does not count as a reference. */
struct computed_table_entry {
  BDDPTR F;		/* misused to hold function (then MSB = 1) */
  BDDPTR G;
  BDDPTR H;
  BDDPTR R;		/* = bdd_ite (F,G,H) or BDD_VOID or F(G,H) */
};

typedef struct computed_table *COMPUTED_TABLE_PTR;

struct computed_table {
  Nat log2size;
  /* Statistics for computed table: */
  int nr_hits;
  int nr_lookups;
  int nr_collisions;
  int nr_items;
  COMPUTED_TABLE_ENTRY entries[1];
};

/* ------------------------------------------------------------------------ */
/* LOCAL VARIABLES                                                          */
/* ------------------------------------------------------------------------ */
static char SccsId[] = "%Z%%Y%/%M% %I% %G%";

static int BDD_PACKAGE_INITIALIZED = 0;

/* GLOBALS FOR BDD ALLOCATION: */

/* Number of BDD nodes allocated at one time:
   (constant nodes are also allocated in a block)
*/
static int bdd_alloc_nodes = 0;

/* Current number bytes allocated since last bdd_init. */
static int bdd_bytes_allocated = 0;

static int bdd_nr_const_nodes = 0;

/* Maximum (ceiling) number bytes allocated since last bdd_init. */
static int bdd_max_bytes_used = 0;

/* Default function to call in case nr. nodes limit is exceeded: */
static BDDPTR bdd_default_nodes_limit_handler (void);

/* User settable variable for nodes-exceeded-function: */
static BDDPTR (*bdd_nodes_limit_handler) (void) =
     bdd_default_nodes_limit_handler;

/* User settable limit for package memory size: */
static int bdd_allowed_memsize = INT_MAX;

/* Default function to call in case memory limit is exceeded: */
static void bdd_default_memfull_handler (void);

/* User settable variable for memory-exceeded-function: */
static void (*bdd_memfull_handler) (void) = bdd_default_memfull_handler;

/* User settable variable for gc hook function: */
static void (*bdd_gc_hook) (void) = NULL;

/* Variables used in managing list of free BDD nodes: */
static BDDPTR bdd_free_list = BDD_VOID;
static const struct bdd BDD_NULL;
static BDDPTR bdd_temp;

static int bdd_sizeof;	/* see bdd_init () */

static int nr_blocks_allocated = 0;
static int nr_nodes_per_block;
static BLKPTR block_free_list = NULL;

/* Initial aux field value: */
static const bdd_aux_field NULL_AUX_FIELD = {0};

UNIQUE_TABLE unique_table;

static COMPUTED_TABLE_PTR computed_table = NULL;

/* ------------------------------------------------------------------------ */
/* EXPORTED VARIABLES                                                       */
/* ------------------------------------------------------------------------ */

/* Do garbage collection if more than 10% of the nodes are dead: */
int bdd_do_gc = 1;

/* Do dynamic variable ordering during gc: */
int bdd_do_dynamic_ordering = 1;

/* Do make use of complemented edges: */
int bdd_use_neg_edges = 1;

/* Do make use of inverted inputs: */
int bdd_use_inv_edges = 1;

/* The BDD package cannot always rely on the fact that the MSB for BDDPTR
   values is 0. Hence it may then not use that bit to record special cases
   in the computed table. Setting this variable to true tells the package
   that it is ok to assume all MSB's will be 0.
*/
int bdd_ok_to_use_MSB = 1;

/* Number of distinct BDD variables created so far: */
int bdd_nr_vars = 0;

/* Number of dead BDD nodes: */
int bdd_nr_dead_nodes = 0;

/* Number of frozen BDD nodes: */
int bdd_nr_frozen_nodes = 0;

/* Maximum (ceiling) number of non-terminal BDD nodes ever present in the
   unique tables (might include some dead nodes) since last bdd_init.
*/
int bdd_peak_nr_nodes = 0;

/* Maximum (ceiling) number of non-terminal BDD nodes ever alive in the
   unique tables (will not include dead nodes) since last bdd_init.
*/
int bdd_peak_nr_nodes_alive = 0;

/* User settable limit for ...???: */
int bdd_size_limit = INT_MAX;

/* User settable limit for total number of BDD nodes: */
int bdd_nr_nodes_allowed = INT_MAX;

/* Size in bytes of extra user space per BDD node: */
int bdd_sizeof_user_data = 0;

/* Total nr. of GC calls since last bdd_init: */
int bdd_nr_gc = 0;

/* Total nr. of DVO calls since last bdd_init: */
int bdd_nr_dynamic = 0;

/* Total nr. of top-level bdd_ite calls since last bdd_init: */
int bdd_nr_ite_calls = 0;

/* The 1-function: */
/*const*/ BDDPTR BDD_1;

/* The 0-function: */
/*const*/ BDDPTR BDD_0;

/* The dontcare-function: */
/* Watch out: BDD_X should never be pointed to by a complemented edge! */
/*const*/ BDDPTR BDD_X;

int bdd_verbose = 0;
int bdd_debug = 0;

int BDD_COMPUTED_TABLE_SIZE = BDD_DEFAULT_COMPUTED_TABLE_SIZE;
int BDD_LOAD_FACTOR         = BDD_DEFAULT_LOAD_FACTOR;

int BDD_HASHTAB_SIZE        = BDD_DEFAULT_HASHTAB_SIZE;
int BDD_NR_RANKS            = BDD_DEFAULT_NR_RANKS;

/* ------------------------------------------------------------------------ */
/* LOCAL FUNCTION PROTOTYPES                                                */
/* ------------------------------------------------------------------------ */

#ifdef BDD_LIGHT
static void bdd_cleanup_sop_cache (void)
{}
#else
extern void bdd_cleanup_sop_cache (void);
#endif

static BDDPTR bdd_ite_aux (BDDPTR F, BDDPTR G, BDDPTR H);

/* ************************************************************************ */
/* FUNCTION DOCUMENTATION:                                                  */
/*                                                                          */
/* ************************************************************************ */

int bdd_nodes_alive (void)
{
  return unique_table.nr_items - bdd_nr_dead_nodes;
}

int bdd_nodes_allocated (void)
{
  return bdd_alloc_nodes;
}

/* See if space limit will not be exceeded: */
static void check_mem_limit (int nr_bytes)
{
  if (bdd_memsize () + nr_bytes > bdd_allowed_memsize)
    bdd_memfull_handler ();
}

#ifdef COMMENT
static void check_for_paging_space (void)
{
  if (psdanger (SIGKILL) < 1000) {
    fprintf (stderr,
 "[check_for_paging_space]: Sorry, not enough free paging space. Aborted.\n");
    exit (1);
  }
}
#endif

/* Allocate another batch (= block) of BDD nodes. */
/* pre: bdd_free_list == NULL */
static BDDPTR bdd_alloc (void)
{
  register BDDPTR p;
  register int i;
  BLKPTR new_blk;

/*  check_for_paging_space ();*/

  check_mem_limit (BDD_BLK_SIZEOF);

  /* Get a fresh block of nodes: */
  new_blk = BDD_CALLOC_BYTES (BDD_BLK_SIZEOF, BLKPTR);
  BDD_BLK_NEXT(new_blk) = block_free_list;
  block_free_list = new_blk;

  nr_blocks_allocated++;

  bdd_alloc_nodes += nr_nodes_per_block;

  /* Link all new nodes in a list: */
  bdd_free_list = p = (BDDPTR) BDD_BLK_SPACE(new_blk);
  for (i = 0; i < nr_nodes_per_block-1; i++) {
    BDDPTR next = (BDDPTR) (((char *) p) + bdd_sizeof);

    p = BDD_NEXT (p) = next;
  }
  BDD_NEXT (p) = NULL;

  /* Strip off the first one: */
  p = bdd_free_list;
  bdd_free_list = BDD_NEXT (bdd_free_list);

  /* In order to misuse the computed table cache for other operations
     then just bdd_ite, it must be assured that all BDD pointers are
     positive, i.e., have a 0 most-significant bit.
  */
  if (BDD_FUNC_P (p)) {
    fprintf (stderr, "[bdd_alloc]: Address of BDD node must have 0 MSB.\n");
    exit (1);
  }

  return p;
}

static void bdd_free_blocks (void)
{
  while (block_free_list) {
    BLKPTR save = block_free_list;

    block_free_list = BDD_BLK_NEXT(block_free_list);
    BDD_FREE_BYTES (save, BDD_BLK_SIZEOF);
  }
  bdd_free_list  = NULL;
  bdd_alloc_nodes = 0;
  bdd_nr_dead_nodes = 0;
  bdd_nr_frozen_nodes = 0;
  nr_blocks_allocated = 0;
}

/* Verify whether BDD pointer is valid. */
int bdd_valid_p (BDDPTR f)
{
  /* Must not be NULL pointer. */
  if (BDD_VOID_P (f))
    return 1;

  /* if !bdd_use_neg_edges then pointer value must have 0 1-st (LSB) bit. */
  if (!bdd_use_neg_edges)
    if (BDD_O_INV_EDGE_P (f))
      return 2;

  /* if !bdd_use_inv_edges then pointer value must have 0 2-nd bit. */
  if (!bdd_use_inv_edges)
    if (BDD_I_INV_EDGE_P (f))
      return 3;

  if (BDD_TERM_P (f))
    return 0;

#ifdef COMMENT
  /* pointer must be in some block. Expensive test! */
  {
    BLKPTR b;

    /* f as real pointer: */
    BDDPTR addr = PTR (f);

    for (b = block_free_list; b; b = BDD_NEXT (b)) {
      /* Address in block of first BDD: */
      BDDPTR first = BDD_BLK_SPACE(b);
      /* Address in block of last BDD: */
      BDDPTR  last =
	(BDDPTR) ((char *) first + bdd_sizeof * (nr_nodes_per_block - 1));

      if (first <= addr && addr <= last) {
	/* Address diff. must be multiple of node size: */
	if ((((char *) addr) - ((char *) first)) % bdd_sizeof)
	  return 5;
	break;
      }
    } /*for*/
    if (!b)
      return 4;
  }
#endif

  /* Non-terminal BDD must not be dead. */
  if (BDD_INTERN_P (f) && BDD_DEAD_P (f))
    return 6;
  return 0;
}

/* Message strings for invalid BDD reasons: */
static const char * const mess[] =
{
  "Invalid reason code",
  "NULL pointer value",
  "Not expecting bit 0 to be set",
  "Not expecting bit 1 to be set",
  "Outside allocated space",
  "Pointer not properly aligned",
  "Pointer to dead non-constant bdd node"
};

/* Fetch BDD-invalid resaon message. */
const char *bdd_invalid_reason (int reason)
{
  if (1 <= reason && reason <= 6)
    return mess[reason];
  return mess[0];
}

/* Verify whether BDD pointer is valid.
   Returns 1 for a valid pointer else prints message including `text'
   to stderr and does exit(1).
*/
int bdd_check_valid (BDDPTR f, char *text)
{
  int reason;

  if ((reason = bdd_valid_p (f)) != 0) {
    fprintf (stderr,
	     "[bdd_check_valid]: %p, %s%s%s.\n",
	     f, mess[reason],
	     text ? ", " : "", text);
    exit (1);
  }
  return 1;
}

void bdd_null_action (BDDPTR v) {}

/* SHOULD NEVER DO ANY ITE OPERATIONS DURING `bdd_traverse_pre'
   (and bdd_traverse_post),
   since the latter uses pointer reversal and might thus severely
   screw up any references to nodes in the BDD being traversed!
*/

/* Acyclic graph traversal. Pre: !BDD_VOID_P (v) */
void bdd_traverse_pre (register BDDPTR v, void (*pre_action) (BDDPTR))
{
  register BDDPTR b = BDD_VOID;
  register BDDPTR t;

 restart:
  BDD_TOGGLE_MARK (v);

  (*pre_action) (v);

  if (BDD_INTERN_P (v)) {
    if (BDD_NOT_MARKED (BDD_THEN (v), v)) {
      MOVE_DOWN_THEN (v);
      goto restart;
    }
  restart2:
    if (BDD_NOT_MARKED (BDD_ELSE (v), v)) {
      MOVE_DOWN_ELSE (v);
      goto restart;
    }
  }

  /* Here: constant or completely handled vertex. */
  while (b)
    MOVE_UP_AND_RESTART2 (v);
}

/* Recursive version of bdd_traverse_pre. */
void bdd_traverse_pre_rec (register BDDPTR v, void (*pre_action) (BDDPTR))
{
  BDD_TOGGLE_MARK (v);

  (*pre_action) (v);

  if (BDD_INTERN_P (v)) {
    BDDPTR T = BDD_THEN (v);
    BDDPTR E = BDD_ELSE (v);

    if (BDD_NOT_MARKED (T, v))
      bdd_traverse_pre_rec (T, pre_action);
    if (BDD_NOT_MARKED (E, v))
      bdd_traverse_pre_rec (E, pre_action);
  }
}

/* Acyclic graph traversal. Pre: !BDD_VOID_P (v) */
void bdd_traverse_post (register BDDPTR v, void (*post_action) (BDDPTR))
{
  register BDDPTR b = BDD_VOID;
  register BDDPTR t;

 restart:
  BDD_TOGGLE_MARK (v);

  if (BDD_INTERN_P (v)) {
    if (BDD_NOT_MARKED (BDD_THEN (v), v)) {
      MOVE_DOWN_THEN (v);
      goto restart;
    }
  restart2:
    if (BDD_NOT_MARKED (BDD_ELSE (v), v)) {
      MOVE_DOWN_ELSE (v);
      goto restart;
    }
  }

  /* Here: constant or completely handled vertex. */
  while (b) {
    (*post_action) (v);
    MOVE_UP_AND_RESTART2 (v);
  }
  (*post_action) (v);
}

/* Recursive version of bdd_traverse_post. */
void bdd_traverse_post_rec (register BDDPTR v, void (*post_action) (BDDPTR))
{
  BDD_TOGGLE_MARK (v);

  if (BDD_INTERN_P (v)) {
    BDDPTR T = BDD_THEN (v);
    BDDPTR E = BDD_ELSE (v);

    if (BDD_NOT_MARKED (T, v))
      bdd_traverse_post_rec (T, post_action);
    if (BDD_NOT_MARKED (E, v))
      bdd_traverse_post_rec (E, post_action);
  }

  /* Here: constant or completely handled vertex. */
  (*post_action) (v);
}

static int node_counter;

static void count_nodes (BDDPTR v)
{
  node_counter++;
}

/* Resets all marks to initial state: */
void bdd_reset_marks (BDDPTR f)
{
  if (!BDD_VOID_P (f))
    bdd_traverse_pre (f, bdd_null_action);
}

#ifdef COMMENT
static void init_aux1_action (BDDPTR v)
{
  BDD_AUX1 (v) = NULL_AUX_FIELD;
}

/* Inits all aux1 fields to 0: */
void bdd_init_aux1_fields (BDDPTR f)
{
  if (!BDD_VOID_P (f))
    bdd_traverse_pre (f, init_aux1_action);
}

static void init_aux2_action (BDDPTR v)
{
  BDD_AUX2 (v) = NULL_AUX_FIELD;
}

/* Inits all aux2 fields to 0: */
void bdd_init_aux2_fields (BDDPTR f)
{
  if (!BDD_VOID_P (f))
    bdd_traverse_pre (f, init_aux2_action);
}
#endif

void bdd_free_aux1_action (BDDPTR v)
{
  bdd_free (BDD_AUX1_BDD (v));
  BDD_AUX1 (v) = NULL_AUX_FIELD;
}

#ifdef COMMENT
void bdd_free_aux2_action (BDDPTR v)
{
  bdd_free (BDD_AUX2_BDD (v));
  BDD_AUX2 (v) = NULL_AUX_FIELD;
}
#endif

void bdd_free_aux1_and_aux2_action (BDDPTR v)
{
  bdd_free (BDD_AUX1_BDD (v));
  bdd_free (BDD_AUX2_BDD (v));
  BDD_AUX1 (v) = NULL_AUX_FIELD;
  BDD_AUX2 (v) = NULL_AUX_FIELD;
}

void bdd_reinit_aux1_action (BDDPTR v)
{
  BDD_AUX1 (v) = NULL_AUX_FIELD;
}

void bdd_reinit_aux1_and_aux2_action (BDDPTR v)
{
  BDD_AUX1 (v) = NULL_AUX_FIELD;
  BDD_AUX2 (v) = NULL_AUX_FIELD;
}

/* Returns number of bdd nodes comprising f.
   Constants have size 1.
   Depending on the use of complemented edges in the BDDs there exist
   either 2 constant nodes or 3.
*/
int bdd_size (BDDPTR f)
{
  node_counter = 0;
  if (!BDD_VOID_P (f)) {
    bdd_traverse_pre (f, count_nodes);
    bdd_reset_marks (f);
  }
  return node_counter;
}

static int global_ceiling;
static void count_nodes_no_consts (BDDPTR v)
{
  if (BDD_INTERN_P (v)) {
    if (node_counter < global_ceiling)
      node_counter++;
    else {
      /* Ceiling reached: cut-off further traversal. */
      if (BDD_NOT_MARKED (BDD_THEN (v), v))
	BDD_TOGGLE_MARK (BDD_THEN (v));
      if (BDD_NOT_MARKED (BDD_ELSE (v), v))
	BDD_TOGGLE_MARK (BDD_ELSE (v));
    }
  }
}

/* Returns number of labelled bdd nodes comprising f.
   Constants are not counted. `ceiling' argument limits the count:
   traversing the bdd stops as soon as this number of nodes is seen.
*/
int bdd_size_ceil (BDDPTR f, int ceiling)
{
  if (ceiling < 1) return 0;
  node_counter = 0;
  global_ceiling = ceiling;
  if (!BDD_VOID_P (f) && BDD_INTERN_P (f)) {
    bdd_traverse_pre (f, count_nodes_no_consts);
    bdd_reset_marks (f);
  }
  return node_counter;
}

int bdd_size_vec (BDDPTR *f_vec, int size)
{
  register int i;

  node_counter = 0;

  /* Assume all mark fields 0. */
  for (i = 0; i < size; i++) {
    BDDPTR f = f_vec[i];

    if (!BDD_VOID_P (f) && !BDD_MARK (f))
      bdd_traverse_pre (f, count_nodes);
  }
  /* Now all mark fields are set. */

  for (i = 0; i < size; i++) {
    BDDPTR f = f_vec[i];

    if (!BDD_VOID_P (f) && BDD_MARK (f))
      bdd_reset_marks (f);
  }
  return node_counter;
}

static int global_max_id;
static int global_max_rank;
static void get_max_var_rank (BDDPTR v)
{
  int rank;

  if (BDD_INTERN_P (v) && (rank = BDD_RANK (v)) > global_max_rank) {
    global_max_rank = rank;
    global_max_id   = BDD_VARID (v);
  }
}

/* Returns variable id with maximum rank value (0-based) among all
   (non-constant) nodes in f.
   If BDD_VOID_P (f) or BDD_TERM_P (f) then returns -1.
*/
int bdd_max_var_rank (BDDPTR f)
{
  if (BDD_VOID_P (f) || BDD_TERM_P (f))
    return -1;

  global_max_rank = 0;
  bdd_traverse_pre (f, get_max_var_rank);
  bdd_reset_marks (f);
  return global_max_id;
}

/* Decrements ref count of f, and when dead afterwards goes on doing this
   repeatedly for then and else functions.

   WATCH OUT! BDD graphs that are still of interest must be protected,
   otherwise when for instance they share nodes with a graph that is
   freed, this bdd_free action will inadvertently corrupt the BDD that
   is supposed to live on.
*/
void bdd_free (BDDPTR f)
{
 restart:
  if (   !BDD_VOID_P (f)
      && !BDD_DEAD_P (f)
      && !BDD_FROZEN_P (f)
      && (--BDD_REFCOUNT (f)) == 0) { 	    /* becomes dead */
    /* 1 more dead node: */
    bdd_nr_dead_nodes++;

    if (BDD_INTERN_P (f)) {
      bdd_free (BDD_THEN (f));
      /* Avoid tail recursion: */
      f = BDD_ELSE (f);
      goto restart;
    }
  }
}

/* Forever protect BDD f from garbage collecting. */
BDDPTR bdd_freeze (BDDPTR f)
{
  if (!BDD_VOID_P (f))
    BDD_FREEZE (f);
  return f;
}

/* bdd_free-s all elements in an array f_vec[0..size-1] of BDDs.
   Does NOT free the space occupied by the array itself.
   No action for a NULL array argument, or size <= 0.
*/
void bdd_free_vec (BDDPTR *f_vec, int size)
{
  BDDPTR *p = f_vec;

  if (!p || size < 0) return;

  while (size--)
    bdd_free (*p++);
}

/* Makes f alive for sure, i.e. increments its ref count and when
   previously dead goes on doing this for then and else functions.
   pre: !BDD_VOID_P (f)

   Open coded for speed.
*/
static void reclaim_aux (BDDPTR f)
{
 restart:
  if (!BDD_VOID_P (f) && !BDD_FROZEN_P (f)) {
    if (BDD_DEAD_P (f)) {
      /* 1 less dead node: */
      bdd_nr_dead_nodes--;
      BDD_REFCOUNT (f) = 1;

      if (BDD_INTERN_P (f)) {
	reclaim_aux (BDD_THEN (f));
	/* Avoid tail recursion: */
	f = BDD_ELSE (f);
	goto restart;
      }
    }
    else
    /* Not frozen and not dead; perhaps becomes frozen: */
    if (++BDD_REFCOUNT (f) == BDD_MAXREFCOUNT)
      bdd_nr_frozen_nodes++;
  }
}

/* Reclaims f, i.e. reclaim_aux its then and else functions assuming f is dead
   and non-constant.
*/
static BDDPTR reclaim (BDDPTR f)
{
  reclaim_aux (BDD_THEN (f));
  reclaim_aux (BDD_ELSE (f));
  if (unique_table.nr_items - bdd_nr_dead_nodes >= bdd_peak_nr_nodes_alive)
    bdd_peak_nr_nodes_alive = unique_table.nr_items - bdd_nr_dead_nodes + 1;
  return f;
}

/* Returns the ceiling of the 2log of n. */
static Nat ceil_log2(Nat n)
{
  Nat mask = 0x0000FFFF;
  Nat not2pow = 0;
  Nat r =  0;
  Nat d = 16;

  /* Fast binary search approach (5 times through loop): */
  do {
    if (n & ~mask) {		 /* n has a 1 bit in left half */
      if (n & mask) not2pow = 1; /* oops, also 1 in right half */
      r += d;
      /* Here: discard right half: */
      n >>= d;
    }
    /* Here: left half is clear. */
    /* Set up a new mask for (right) half of the original right half: */
    d >>= 1;
    mask >>= d;
  } while (d);
  /* Here: n is either 0 or 1. */
  return r + not2pow;
}

/* Using muliplicative method in hashing. Constant A according Knuth:
   A = (PHI - 1) * 2^32, with PHI = golden ratio = (1 + sqrt(5))/2
   PHI-1 = 0.61803 39887 49894 84820 ...
   A = 2654435769 = -1640531527 = 0x9E3779B9U
   The nearest prime number is 2654435761 = 0x9E3779B1U
*/

/* Hashes Nat `k' to a value in the range [0..2^log2size-1]. */
#define BDD_HASH(k,log2size)	div_pow2(0x9E3779B1U*(k), NAT_BIT-(log2size))

#define hash_U(T, E, log2size) \
     BDD_HASH((Nat)((BITS (T) >> 2) ^ (BITS (E) << 3)), (log2size))

/*  ((((v) ^ ((int) T << 7) ^ ((int) (E) << 9)) & INT_MAX) % size)*/

#define hash_C(F, G, H, log2size) \
     BDD_HASH((Nat)(BITS (F) ^ (BITS (G) << 7) ^ (BITS (H) << 9)), (log2size))

static
BDDPTR bdd_lookup_computed_table_no_reclaim (BDDPTR F, BDDPTR G, BDDPTR H)
{
  int index = hash_C (F, G, H, BDD_CT_LOG2SIZE(computed_table));
  COMPUTED_TABLE_ENTRY *entry = BDD_CT_ENTRIES(computed_table) + index;
  BDDPTR R;

  BDD_CT_LOOKUPS(computed_table)++;

  R = entry->R;

  if (   !BDD_VOID_P (R)
      && BDD_EQUAL_P (F, entry->F)
      && BDD_EQUAL_P (G, entry->G)
      && BDD_EQUAL_P (H, entry->H)) {
    BDD_CT_HITS(computed_table)++;
    return R;
  }
  return BDD_VOID;
}

static BDDPTR bdd_lookup_computed_table (BDDPTR F, BDDPTR G, BDDPTR H)
{
  BDDPTR R = bdd_lookup_computed_table_no_reclaim (F, G, H);

  return !BDD_VOID_P (R) && BDD_DEAD_P (R) && BDD_INTERN_P (R)
    ? reclaim (R) : R;
}

/* Returns result R. */
static BDDPTR bdd_insert_computed_table (BDDPTR F, BDDPTR G, BDDPTR H,
					 BDDPTR R)
{
  int index = hash_C (F, G, H, BDD_CT_LOG2SIZE(computed_table));
  COMPUTED_TABLE_ENTRY *entry = BDD_CT_ENTRIES(computed_table) + index;

#ifdef COMMENT
  /* Who cares? Just count as a collision. */
  if (   !BDD_VOID_P (entry->R)
      && BDD_EQUAL_P (F, entry->F)
      && BDD_EQUAL_P (G, entry->G)
      && BDD_EQUAL_P (H, entry->H))
    /* Already present. */
    return R;
#endif

  entry->F = F;
  entry->G = G;
  entry->H = H;

  if (BDD_VOID_P (entry->R))
    /* A fresh entry: */
    BDD_CT_ITEMS(computed_table)++;
  else
    BDD_CT_COLLS(computed_table)++;

  return entry->R = R;
}

/* Creates an empty hash table for BDD nodes with variable id `varid'.
   Number of empty entries allocated is pow2(log2size).
*/
static V_HASHTAB bdd_make_hash_table (int varid, int log2size)
{
  V_HASHTAB new_hash_table;
  Nat nr_bytes = BDD_VUT_SIZEOF (log2size);

  check_mem_limit (nr_bytes);

  new_hash_table = BDD_CALLOC_BYTES (nr_bytes, V_HASHTAB);

  BDD_VUT_ID(new_hash_table) = varid;

  /* Number of allocated entries: */
  BDD_VUT_LOG2SIZE(new_hash_table) = log2size;

  /* Number of items currently held by the hash table: */
  BDD_VUT_ITEMS(new_hash_table) = 0;

  return new_hash_table;
}

/* Resizes the hash_table.
   Updates the unique table as a side-effect.
   Returns the new table.
*/
static V_HASHTAB bdd_resize_hash_table (V_HASHTAB hash_table, int extend)
{
  int          v = BDD_VUT_ID(hash_table);
  int       rank = BDD_VAR_RANK (v);
  int   log2size = BDD_VUT_LOG2SIZE(hash_table);
  int o_log2size = log2size;
  int     o_size = pow2(o_log2size);
  V_HASHTAB o_hash_table;

  /* Save old hash_table: */
  o_hash_table = hash_table;

  /* Get new size: */
  if (extend) {

#if defined(BDD_DEBUG)
    fprintf (stderr,
	     "[bdd]: Extending table %d [rank:%d] (%d nodes)...",
	     v, rank, BDD_VUT_ITEMS(hash_table));
#endif

    log2size++;

    /* Two hash tables temporarily coexist; so let's be honest
       and check for the size of the new (bigger) one:
    */
    check_mem_limit (BDD_VUT_SIZEOF (log2size));
  }
  else {
#ifdef COMMENT
    int preferred_size;

    /* Get the smallest table that ensures the proper load factor.
       However, don't go below the standard BDD_HASHTAB_SIZE.
       Otherwise too much shrinking and extending would result.
    */
    preferred_size = BDD_VUT_ITEMS(o_hash_table) / BDD_LOAD_FACTOR;
    if (preferred_size < BDD_HASHTAB_SIZE)
      preferred_size = BDD_HASHTAB_SIZE;

    log2size = ceil_log2 (preferred_size);

    if (BDD_VUT_LOG2SIZE(o_hash_table) <= log2size)
      return o_hash_table;      
#endif

    if (BDD_VUT_LOG2SIZE(o_hash_table) <= 1)
      /* Don't shrink to dummy table. */
      return o_hash_table;

    log2size--;

#if defined(BDD_DEBUG)
    fprintf (stderr,
	     "[bdd]: Shrinking table %d [rank:%d] (%d nodes)...",
	     v, rank, BDD_VUT_ITEMS(hash_table));
#endif
  }

  hash_table = BDD_CALLOC_BYTES (BDD_VUT_SIZEOF (log2size), V_HASHTAB);
  BDD_VUT_ID      (hash_table) = v;
  BDD_VUT_LOG2SIZE(hash_table) = log2size;

  /* Rehash all the old entries (dead nodes inclusive): */
  {
    register int i;
    register BDDPTR *o_entry = BDD_VUT_ENTRIES(o_hash_table);
    BDDPTR *entries = BDD_VUT_ENTRIES(hash_table);

    for (i = 0; i < o_size; i++, o_entry++) {
      BDDPTR node = *o_entry;

      while (node) {
	BDDPTR  next = BDD_NEXT (node);
	BDDPTR     T = BDD_THEN (node);
	BDDPTR     E = BDD_ELSE (node);
	int    index = hash_U (T, E, log2size);
	BDDPTR *spot = entries + index;

	BDD_NEXT (node) = *spot;
	*spot = node;

	node = next;
      } /*while*/
    } /*for*/
  }
  BDD_VUT_ITEMS(hash_table) = BDD_VUT_ITEMS(o_hash_table);

  BDD_FREE_BYTES (o_hash_table, BDD_VUT_SIZEOF (o_log2size));

  if (rank == BDD_TERMID)
    unique_table.terms = hash_table;
  else
    unique_table.space[rank] = hash_table;
  unique_table.nr_entries += pow2(log2size) - pow2(o_log2size);

#if defined(BDD_DEBUG)
  fprintf (stderr, "done.\n");
#endif

  return hash_table;
}

/* Make sure that there is a hash table for variable `v'.
   Will extend unique table and create hash tables if necessary.
   Assumes initial unique_table has been allocated.
*/
static V_HASHTAB bdd_make_fit_unique_table (int v)
{
  V_HASHTAB tab;
  int rank;

  if (v > CURR_MAXRANK) {
    int size = unique_table.size;

    if (v >= size) {
      int new_size = size;

      do
	new_size += size;
      while (v >= new_size);
      /* v < new_size */

      check_mem_limit ((new_size - size) * (  sizeof (V_HASHTAB)
					    + sizeof (int)
					    + sizeof (GROUP_REC)));

      unique_table.space  = BDD_REALLOC_ARRAY (unique_table.space,
					       new_size,
					       V_HASHTAB,
					       size);

      unique_table.ranks  = BDD_REALLOC_ARRAY (unique_table.ranks,
					       new_size,
					       int,
					       size);

      unique_table.groups = BDD_REALLOC_ARRAY (unique_table.groups,
					       new_size,
					       GROUP_REC,
					       size);

      /* Clear the extra space: */
      memset (unique_table.space + size, 0,
	      (new_size - size) * sizeof (V_HASHTAB));

      unique_table.size = new_size;

    } /*if*/

    /* Init the extra ranks and groups: */
    for (; unique_table.count <= v; unique_table.count++) {
      int g = NR_GROUPS++;

      GROUP_ORDERABLE(g) = 0;
      GROUP_LAST_RANK(g) =
      unique_table.ranks[unique_table.count] = unique_table.count;

      tab = bdd_make_hash_table (unique_table.count, 0);
      unique_table.space[unique_table.count] = tab;
      unique_table.nr_entries += BDD_VUT_SIZE(tab);
    }
    /* unique_table.count = v + 1 */
  } /*if*/

  rank = unique_table.ranks[v];
  tab  = unique_table.space[rank];
  if (!BDD_VUT_LOG2SIZE(tab)) {	/* used to be dummy table */
    bdd_nr_vars++;

#if defined(BDD_DEBUG)
    fprintf (stderr,
	     "New variable id %d with current rank %d (total %d vars).\n",
	     v, rank, bdd_nr_vars);
#endif

    tab = bdd_resize_hash_table (tab, 1);
  }
  return tab;
}

/* Creates an initial unique table. */
static UNIQUE_TABLE bdd_make_unique_table (void)
{
  UNIQUE_TABLE unique_table;

  /* Current maximum size (i.e. number of ranks) of unique table: */
  unique_table.size = BDD_NR_RANKS;

  /* This reflects the number of ranks currently held by the unique table: */
  unique_table.count = 0;

  /* Total nr entries allocated for ranks: */
  unique_table.nr_entries = 0;

  /* This reflects the number of nodes currently held by the unique table: */
  unique_table.nr_items = 0;

  /* Total nr groups: */
  unique_table.nr_groups = 0;

  /* Allocate an array of empty entries: */

  check_mem_limit (BDD_NR_RANKS * (  sizeof (V_HASHTAB)
				   + sizeof (int)
				   + sizeof (GROUP_REC)));

  unique_table.space  = BDD_CALLOC_ARRAY (BDD_NR_RANKS, V_HASHTAB);
  unique_table.ranks  = BDD_CALLOC_ARRAY (BDD_NR_RANKS, int);
  unique_table.groups = BDD_CALLOC_ARRAY (BDD_NR_RANKS, GROUP_REC);
  unique_table.terms  = NULL;

  return unique_table;
}

/* Creates an initial computed table. */
static void bdd_make_computed_table (int size)
{
  Nat log2size = ceil_log2 (size);
  Nat nr_bytes = BDD_CT_SIZEOF (log2size);

  check_mem_limit (nr_bytes);

  computed_table = BDD_CALLOC_BYTES (nr_bytes, COMPUTED_TABLE_PTR);
  BDD_CT_LOG2SIZE(computed_table) = log2size;
/* because of CALLOC:
  BDD_CT_HITS   (computed_table) = 0;
  BDD_CT_LOOKUPS(computed_table) = 0;
  BDD_CT_COLLS  (computed_table) = 0;
  BDD_CT_ITEMS  (computed_table) = 0;
*/
}

/* Scan through computed table for any DEAD node references.
   Makes those entries invalid.

   Pre: computed_table != NULL.
*/
static void bdd_flush_computed_table (void)
{
  register int i;
  register int size = BDD_CT_SIZE(computed_table);
  register COMPUTED_TABLE_ENTRY *entry = BDD_CT_ENTRIES(computed_table);

  for (i = 0; i < size; i++, entry++) {
    BDDPTR R = entry->R;

    if (   !BDD_VOID_P (R)
	&& (   (!BDD_FUNC_P (entry->F) && BDD_DEAD_P (entry->F))
	    || BDD_DEAD_P (entry->G)
	    || BDD_DEAD_P (entry->H)
	    || BDD_DEAD_P (R))) {
      entry->R = BDD_VOID;
      BDD_CT_ITEMS(computed_table)--;
    }
  } /*for*/
}

/* Clear all entries in the computed table. */
static void bdd_clear_computed_table (void)
{
  if (BDD_CT_ITEMS(computed_table)) {
    memset (BDD_CT_ENTRIES(computed_table), 0,
	    BDD_CT_SIZE(computed_table) * sizeof (COMPUTED_TABLE_ENTRY));
    BDD_CT_ITEMS(computed_table) = 0;
  }
}

/* Checks whether computed table needs extending and does so.
   Always makes sure any DEAD node references are invalidated.

   Pre: computed_table != NULL.
*/
static void bdd_extend_computed_table (void)
{
  if (   ((float) unique_table.nr_entries) > 2 * BDD_CT_SIZE(computed_table)
      && BDD_CT_LOG2SIZE(computed_table) < 20) {
    int i;
    int   log2size = BDD_CT_LOG2SIZE(computed_table);
    int o_log2size = log2size;
    int     o_size = pow2(o_log2size);
    COMPUTED_TABLE_PTR o_computed_table;
    COMPUTED_TABLE_ENTRY *entry = BDD_CT_ENTRIES(computed_table);
    Nat nr_bytes;

    if (bdd_verbose)
      fprintf (stderr, "[bdd]: Extending computed table...");

    /* Save old computed_table: */
    o_computed_table = computed_table;

    /* Get new size: */
    log2size++;

    /* Two computed tables temporarily coexist; so let's be honest
       and check for the size of the new (bigger) one:
    */
    nr_bytes = BDD_CT_SIZEOF (log2size);

    check_mem_limit (nr_bytes);

    computed_table = BDD_CALLOC_BYTES (nr_bytes, COMPUTED_TABLE_PTR);
    BDD_CT_LOG2SIZE(computed_table) = log2size;
    BDD_CT_ITEMS   (computed_table) = 0;
    BDD_CT_HITS    (computed_table) = BDD_CT_HITS   (o_computed_table);
    BDD_CT_LOOKUPS (computed_table) = BDD_CT_LOOKUPS(o_computed_table);
    BDD_CT_COLLS   (computed_table) = BDD_CT_COLLS  (o_computed_table);

    /* Rehash all the old entries: */
    for (i = 0; i < o_size; i++, entry++) {
      BDDPTR R = entry->R;

      if (   !BDD_VOID_P (R)
	  && !BDD_DEAD_P (R)
	  && (BDD_FUNC_P (entry->F) || !BDD_DEAD_P (entry->F))
	  && !BDD_DEAD_P (entry->G)
	  && !BDD_DEAD_P (entry->H)) {
	bdd_insert_computed_table (entry->F, entry->G, entry->H, R);
      } /*if*/
    } /*for*/

    BDD_FREE_BYTES (o_computed_table, BDD_CT_SIZEOF (o_log2size));

    if (bdd_verbose)
      fprintf (stderr, "done.\n");
  }
}

static void print_unique_table_stats (FILE *fp)
{
  int nr_entries  = unique_table.nr_entries;
  int items       = unique_table.nr_items;
  int nr_vars     = unique_table.count;
  /* Average items per bucket: */
  int load_factor = nr_entries ? items / nr_entries : 0;
  int balance = 0;
  int i;
  int maxk = 0;

  fprintf (fp, "*** BDD Unique-Table Info");

  for (i = 0; i < unique_table.count; i++) {
    V_HASHTAB tab = unique_table.space[i];
    unsigned int j;

    for (j = 0U; j < BDD_VUT_SIZE(tab); j++) {
      int k = 0;
      BDDPTR chain = BDD_VUT_ENTRIES(tab)[j];

      for (; chain; chain = BDD_NEXT (chain), k++);
      k -= load_factor;
      if (k < 0) k = -k;
      if (k > maxk) maxk = k;
      balance += k;
    }
  }

  fprintf (fp, " (%d hash tables, %d groups) ***\n",
	   nr_vars, unique_table.nr_groups);

  fprintf (fp,
   "Nodes: %d, use: %d (%d alive,%d dead,%d frozen); peak: %d.\n",
	   bdd_alloc_nodes, items, items - bdd_nr_dead_nodes,
	   bdd_nr_dead_nodes, bdd_nr_frozen_nodes, bdd_peak_nr_nodes);

  i = nr_blocks_allocated * (BDD_BLK_SIZEOF / 1024);
  fprintf (fp, "Memory: Nodes: %d,", i);
  i = (  nr_entries * sizeof (BDDPTR)
       + unique_table.size * (  sizeof (V_HASHTAB)
			      + sizeof (int)
			      + sizeof (GROUP_REC))
      ) / 1024;
  fprintf (fp, " U-tab: %d,", i);
  i = computed_table ? (BDD_CT_SIZEOF (BDD_CT_LOG2SIZE(computed_table)))
                       / 1024
		     : 0;
  fprintf (fp, " C-tab: %d,", i);
  fprintf (fp, " Tot: %d, Peak: %d (kb).\n",
	   bdd_memsize () / 1024, bdd_max_bytes_used / 1024);
/*
  fprintf (fp, "#entries: %d, load: %d, Off: %d%%, Max: %d.\n",
	   nr_entries, load_factor, items ? balance * 100 / items : 0, maxk);
*/
  fprintf (fp, "Top-Level ITE calls             : %6d.\n",
	   bdd_nr_ite_calls);
  fprintf (fp, "Garbage Collection calls        : %6d.\n", bdd_nr_gc);
  fprintf (fp, "Dynamic Variable Ordering calls : %6d.\n",
	   bdd_nr_dynamic);
}

static int bdd_gc_aux (void)
{
  register int i;
  register int total_nr_nodes_freed = 0;
  register V_HASHTAB *tab = unique_table.space;

  bdd_nr_gc++;

  if (bdd_gc_hook) bdd_gc_hook ();

/*
  if (bdd_verbose)
    fprintf (stderr, "[bdd_gc]: Cleaning SOP cache...");
*/
  /* Not really necessary: */
  bdd_cleanup_sop_cache ();
/*
  if (bdd_verbose)
    fprintf (stderr, "done.\n");
*/
  for (i = 0; i < unique_table.count; i++, tab++) {

    if (*tab) {			/* table for rank i vars exists */
      register int j;
      register int size = BDD_VUT_SIZE(*tab); /* entries allocated */
      register BDDPTR *entry = BDD_VUT_ENTRIES(*tab);
      register int nr_nodes_freed = 0;

      for (j = 0; j < size; j++, entry++) {
	register BDDPTR *chain = entry;
	register BDDPTR  node  = *chain;

	while (node) {
	  if (BDD_DEAD_P (node)) {
	    *chain = BDD_NEXT (node);
	    BDD_FREE (node);
	    nr_nodes_freed++;
	  }
	  else
	    chain = &(BDD_NEXT (node));
	  node = *chain;
	} /*while*/
      } /*for*/
      total_nr_nodes_freed += nr_nodes_freed;
      BDD_VUT_ITEMS(*tab) -= nr_nodes_freed;

      /* Check whether table can be shrunk: (quarter of the load factor) */
      if (BDD_VUT_ITEMS(*tab) < (size >> 2) * BDD_LOAD_FACTOR)
	bdd_resize_hash_table (*tab, 0);

    } /*if*/
  } /*for i*/
  unique_table.nr_items -= total_nr_nodes_freed;

  return total_nr_nodes_freed;
}

/* Explicit attempt at collecting DEAD BDD nodes and reclaiming the space they
   occupy. Also flushes any computed table entries that refer to dead nodes.
   Returns number of BDD nodes actually freed.
*/
int bdd_gc (void)
{
  int total_nr_nodes_freed;

  if (bdd_verbose)
    fprintf (stderr, "[bdd_gc]: Garbage collecting (%d dead nodes)...",
	     bdd_nr_dead_nodes);

  /* Make sure that dead nodes contained in the computed_table
     can no longer be reclaimed, i.e. invalidate any entry that has
     at least 1 dead node. This has to be called before the dead
     BDD nodes are freed!
  */
  bdd_flush_computed_table ();

  total_nr_nodes_freed = bdd_gc_aux ();

  if (bdd_verbose)
    fprintf (stderr, "done (%d nodes freed).\n", total_nr_nodes_freed);

  return total_nr_nodes_freed;
}

/* pre: F is (positive) non-constant function. */
#define MATCHES(F, v, T, E)	(   \
				    (BDD_EQUAL_P (BDD_THEN (F), T)) \
				 && (BDD_EQUAL_P (BDD_ELSE (F), E)))

#ifdef COMMENT
/* Tests whether the triple <v,T,E> is present as a BDD node in the
   unique table, assuming that nodes for T and E already exist.

   Pre: v != BDD_TERMID.
*/
static int bdd_node_present (int v, BDDPTR T, BDDPTR E)
{
  V_HASHTAB tab;

  if (BDD_VOID_P (T) || BDD_VOID_P (E))
    return 0;

  if (BDD_EQUAL_P (T, E))
    return 1;

  if (v < 0)
    v = -v;

  {
    int rank = BDD_VAR_RANK (v);

    if (   rank < 0
	|| rank > CURR_MAXRANK
	|| !(tab = unique_table.space[rank]))
      return 0;
  }

  if (BDD_O_INV_EDGE_P (T)) {
    T = BDD_O_OFF (T);
    E = BDD_COMPL (E);
  }

  if (bdd_use_inv_edges && !BDD_O_INV_EDGE_P (E) && T > E) {
    BDDPTR R;

    R = T;
    T = E;
    E = R;
  }

  /* Here: !BDD_VOID_P (T) && !BDD_VOID_P (E),
           T < E,
           T positive,
	   T != E,
	   0 <= v < BDD_TERMID.
  */
  {
    int index = hash_U (T, E, BDD_VUT_LOG2SIZE(tab));
    register BDDPTR chain;

    /* Lookup in bucket: */
    for (chain = BDD_VUT_ENTRIES(tab)[index]; chain; chain = BDD_NEXT (chain))
      if (MATCHES (chain, v, T, E))
	return 1;
  }
  return 0;
}
#endif

/* There are 4 possible configurations in case of negated edges:

   f1 =  v.T + v'.E,
   f2 = (v.T + v'.E)',
   f3 =  v.T + v'.E',
   f4 = (v.T + v'.E')'.

   For sake of canonicity, in all 4 we see that the THEN edge `T'
   always appears positive.
   Of these, only cases 1 and 2 are allowed to also have inverted inputs:

   f1 =  v.T + v'.E   =  (v').E + (v')'.T,
   f2 = (v.T + v'.E)' = ((v').E + (v')'.T)'.

   Which configuration to use is uniquely determined by requiring
   that for the pointer values T < E holds.
   This is based on the fact that initially we take care that
   BDD_1 < BDD_0 holds so a BDD projection function for a positive
   variable will have the inverted-input bit cleared.
   Constants cannot have the inverted-input bit on!

   The following table lists all possible negated-output (O)/invert-input (I)
   combinations that are possible for a non-constant BDD pointer and gives
   the invariants for its node's then- and else-field pointers: + means that
   the pointer value must be positive, i.e. the negated-output bit is off.

   I | O || T ~ E
   --+---++------
   0 | 0 || T+
   0 | 1 || T+
   1 | 0 || T+, E+, T < E
   1 | 1 || T+, E+, T < E

   For BDD pointers to constant nodes the following holds:

   I | O || BDD_1 | BDD_0 | BDD_X
   --+---++-------+-------+------
   0 | 0 ||  ok   |  ok   |  ok
   0 | 1 ||  -    |  ok   |  -
   1 | 0 ||  -    |  -    |  -
   1 | 1 ||  -    |  -    |  -

   The column for BDD_0 has two ok entries because when using
   negated outputs it is the complement of BDD_1. A dash - entry
   means "not possible".
*/

/* Pre: v != BDD_TERMID, !BDD_VOID_P (T) && !BDD_VOID_P (E),
        rank(v) < rank (T) && rank(v) < rank (E).

   Note: always increments T and E nodes' ref count by 1.
   Return value might have bit0 and/or bit1 set!
*/
static BDDPTR bdd_create_node (int v, BDDPTR T, BDDPTR E)
{
  int neg_result = 0;
  int inv_result = 0;
  int rank;
  V_HASHTAB tab;

  if (BDD_EQUAL_P (T, E)) {
    BDD_INCR_REF (T);
    return T;
  }

  if (v < 0) {
    BDDPTR R;

    /* < v, T, E > ==> < -v, E, T > */
    v = -v;
    R = T;
    T = E;
    E = R;
  }

  /* Here: !BDD_VOID_P (T) && !BDD_VOID_P (E),
	   T != E,
	   0 <= v < BDD_TERMID.
  */

  if (BDD_TERM_P (T) && !BDD_BOOL_P (T)) {
    if (BDD_O_INV_EDGE_P (E)) {
      neg_result = 1;
      E = BDD_O_OFF (E);
    }
  }
  else
  if (BDD_O_INV_EDGE_P (T)) {
    neg_result = 1;
    T = BDD_O_OFF (T);
    E = BDD_COMPL (E);
  }

  /* Here: !BDD_VOID_P (T) && !BDD_VOID_P (E),
           T positive,
	   T != E,
	   0 <= v < BDD_TERMID.
  */

  if (bdd_use_inv_edges && !BDD_O_INV_EDGE_P (E) && T > E) {
    BDDPTR R;

    inv_result ^= 1;
    R = T;
    T = E;
    E = R;
  }

  /* Here: !BDD_VOID_P (T) && !BDD_VOID_P (E),
           T < E,
           T positive,
	   T != E,
	   0 <= v < BDD_TERMID.
  */
  tab  = bdd_make_fit_unique_table (v);
  rank = BDD_VAR_RANK (v);

  if (rank < BDD_RANK (T) && rank < BDD_RANK (E)) {
    int log2size = BDD_VUT_LOG2SIZE(tab);
    int index = hash_U (T, E, log2size);
    register BDDPTR new;

    /* Lookup in bucket: */
    for (new = BDD_VUT_ENTRIES(tab)[index]; new; new = BDD_NEXT (new))
      if (MATCHES (new, v, T, E)) {
	if (BDD_DEAD_P (new)) {
	  BDD_INCR_REF (T);
	  BDD_INCR_REF (E);

	  if (unique_table.nr_items - bdd_nr_dead_nodes
	      >= bdd_peak_nr_nodes_alive)
	    bdd_peak_nr_nodes_alive =
	      unique_table.nr_items - bdd_nr_dead_nodes + 1;
	}
	/* new is non-constant. */
	/* new has modification bits off. */
	BDD_INCR_REF (new);
	if (inv_result) new = BDD_I_SET_U (new);
	if (neg_result) new = BDD_O_SET_U (new);
	return new;
      } /*if-for*/

    /* Must create a new entry. */

    /* See if size is bounded, and user wants partial BDD: */
    if (unique_table.nr_items >= bdd_size_limit)
      return bdd_assign (BDD_X);

    /* See if allowed to allocate a new node: */
    if (unique_table.nr_items >= bdd_nr_nodes_allowed)
      return bdd_nodes_limit_handler ();

    new            = BDD_MALLOC ();
    /* new of course has modification bits off. */
    new->varid     = v;
    new->mark      = 0;
    new->flag      = 0;
    /* Protect new from gc: */
    new->refcount  = 1;
    /* Let's be nice and ensure that aux fields are initialized: */
    BDD_AUX1 (new) = NULL_AUX_FIELD;
    BDD_AUX2 (new) = NULL_AUX_FIELD;
    new->then_link = T;
    BDD_INCR_REF (T);
    new->else_link = E;
    BDD_INCR_REF (E);

    /* Link to unique table: */
    BDD_NEXT (new) = BDD_VUT_ENTRIES(tab)[index];
    BDD_VUT_ENTRIES(tab)[index] = new;
    BDD_VUT_ITEMS(tab)++;
    if (++unique_table.nr_items > bdd_peak_nr_nodes)
      bdd_peak_nr_nodes++;

    if (unique_table.nr_items - bdd_nr_dead_nodes > bdd_peak_nr_nodes_alive)
      bdd_peak_nr_nodes_alive = unique_table.nr_items - bdd_nr_dead_nodes;

    /* See if >12.5% is dead: */
    if (   bdd_nr_dead_nodes > (unique_table.nr_items >> 3)
	/* Avoid too many gc calls. */
	&& unique_table.nr_items > 4096
	&& bdd_do_gc)
      bdd_gc ();

    /* Earlier call to bdd_dynamic_order might have modified tables
       so tab cannot be relied upon. Size may also have shrunk.
    */
    tab = unique_table.space[BDD_RANK (new)];
    log2size = BDD_VUT_LOG2SIZE(tab);

    /* Determine load factor: */
    if (((float) BDD_VUT_ITEMS(tab)) / pow2(log2size) > BDD_LOAD_FACTOR) {

      if (bdd_verbose) {
	print_unique_table_stats (stderr);
	fprintf (stderr, "[bdd]: Load factor exceeds %d.\n",
		 BDD_LOAD_FACTOR);
      }

      bdd_resize_hash_table (tab, 1);
      bdd_extend_computed_table ();
    }
    /* new is non-constant. */
    /* new has modification bits off. */
    if (inv_result) new = BDD_I_SET_U (new);
    if (neg_result) new = BDD_O_SET_U (new);
    return new;
  }

  /* Here cannot simply create node <v, T, E> because of the rank conflict.
     Solution: let ite solve it by calculating ite (v, T, E).
     Better avoid doing dynamic ordering in that case because usually
     that is precisely why we couldn't simply create the node in the
     first place.

     Note: will never get here when dynamic variable ordering does not
     occur inside a bdd_ite_aux call.
  */
  {
    int save_bdd_do_dynamic_ordering;
    BDDPTR R;
    BDDPTR V = bdd_create_var (v);

    save_bdd_do_dynamic_ordering = bdd_do_dynamic_ordering;
    bdd_do_dynamic_ordering = 0;

    R = bdd_ite_aux (V, T, E);
    bdd_free (V);

    bdd_do_dynamic_ordering = save_bdd_do_dynamic_ordering;

    return R;
  }
}

#if 0
void (*bdd_user_terminal_free_func) (void *data) = NULL;
void (*bdd_user_terminal_print_func) (FILE fp, void *data) = NULL;

/* Creates a user terminal BDD node. The data is used as hash key and stored
   on the node's then link; the else link will always be BDD_VOID.

   Use the macro:

   BDD_USER_TERM_P (F)
   BDD_USER_TERM_DATA (F)
*/
BDDPTR bdd_create_user_terminal (void *data)
{
  V_HASHTAB tab;
  int  log2size;
  int     index;
  register BDDPTR new;

  if (!data)
    return bdd_0 ();

  if (data == (void *) 1)
    return bdd_1 ();

  if (!(tab = unique_table.terms))
    tab = unique_table.terms = bdd_make_hash_table (BDD_TERMID, 0);

  log2size = BDD_VUT_LOG2SIZE(tab);
  index = hash_U (data, 0, log2size);

  /* Lookup in bucket: */
  for (new = BDD_VUT_ENTRIES(tab)[index]; new; new = BDD_NEXT (new))
    if ((BDDPTR) data == BDD_THEN (new)) {
      /* new has modification bits off. */
      BDD_INCR_REF (new);
      return new;
    } /*if-for*/

  /* Must create a new entry. */

  /* See if allowed to allocate a new node: */
  if (unique_table.nr_items >= bdd_nr_nodes_allowed)
    return bdd_nodes_limit_handler ();

  new            = BDD_MALLOC ();
  /* new of course has modification bits off. */
  new->varid     = BDD_TERMID;
  new->mark      = 0;
  new->flag      = 0;
  /* Protect new from gc: */
  new->refcount  = 1;
  /* Let's be nice and ensure that aux fields are initialized: */
  BDD_AUX1 (new) = NULL_AUX_FIELD;
  BDD_AUX2 (new) = NULL_AUX_FIELD;
  new->then_link = (BDDPTR) data;
  new->else_link = BDD_VOID;

  /* Link to unique table's terms hash table: */
  BDD_NEXT (new) = BDD_VUT_ENTRIES(tab)[index];
  BDD_VUT_ENTRIES(tab)[index] = new;
  BDD_VUT_ITEMS(tab)++;
  if (++unique_table.nr_items > bdd_peak_nr_nodes)
    bdd_peak_nr_nodes++;

  if (unique_table.nr_items - bdd_nr_dead_nodes > bdd_peak_nr_nodes_alive)
    bdd_peak_nr_nodes_alive = unique_table.nr_items - bdd_nr_dead_nodes;

  /* See if >12.5% is dead: */
  if (   bdd_nr_dead_nodes > (unique_table.nr_items >> 3)
      /* Avoid too many gc calls. */
      && unique_table.nr_items > 4096
      && bdd_do_gc)
    bdd_gc ();

  /* Determine load factor: */
  if (((float) BDD_VUT_ITEMS(tab)) / pow2(log2size) > BDD_LOAD_FACTOR) {

    if (bdd_verbose) {
      print_unique_table_stats (stderr);
      fprintf (stderr, "[bdd]: Load factor exceeds %d.\n",
	       BDD_LOAD_FACTOR);
    }

    bdd_resize_hash_table (tab, 1);
  }
  return new;
}
#endif

/* Returns id of variable that is assigned the order number `rank'.
   (`rank' is the 0-based user view of a rank value.)
   If not found, -1 is returned.
   rank numbers and indices are always assumed >= 0.
*/
int bdd_var_rank_to_id (int rank)
{
  if (   rank < 0		/* invalid rank number */
      || rank > CURR_MAXRANK)	/* beyond max rank number */
    return -1;
  return BDD_VUT_ID(unique_table.space[rank]);
}

/* Returns rank number of variable that is known under id, i.e., the 0-based
   position of that variable in the ordering of variables.
   If id is invalid or non-existent, -1 is returned.
   rank numbers and indices are always assumed >= 0.
*/
int bdd_var_id_to_rank (int id)
{
  if (   id < 0			/* invalid id number */
      || id > CURR_MAXRANK)	/* beyond max variable id */
    return -1;
  return BDD_VAR_RANK (id);
}

static int bdd_var_id_to_group (int id)
{
  int rank = bdd_var_id_to_rank (id);
  int g;

  if (rank < 0) return -1;

  for (g = 0; g < NR_GROUPS; g++)
    if ((Nat) rank <= GROUP_LAST_RANK(g))
      return g;
  return -1;
}

/* Returns the current number of BDD nodes (dead, alive, or frozen) that
   have `id' in their variable id field.
*/
int bdd_nr_occurs_var (int id)
{
  return (id < 0 || id > CURR_MAXRANK) ? 0 :
    BDD_VUT_ITEMS(unique_table.space[BDD_VAR_RANK (id)]);
}

/* Creates a BDD for the variable with id v.
   Variables are identified by integer numbers upto and including MAXVARID
   Best is to let variable numbers start with 1, that way negative values
   may be used to denote the negative literal for that variable.
   Variable 0 should therefore not be used.
   Note that the index field of a BDD node will always be positive,
   complements are normally achieved through the use of negative edges.

   Example:
   		bdd_create_var (3)

		creates the BDD:
                         |
                         |
		       ( 3 )
		       /   o
		    1 /     \ 0
		      \     /
		       [ 1 ]

   		bdd_create_var (-1)

		creates the BDD:
                         o
                         |
		       ( 1 )
		       /   o
		    1 /     \ 0
		      \     /
		       [ 1 ]
*/
BDDPTR bdd_create_var (int v)
{
  int id;

  if (v == -BDD_TERMID) {
    BDD_INCR_REF (BDD_0);
    return BDD_0;
  }

  if (v == BDD_TERMID) {
    BDD_INCR_REF (BDD_1);
    return BDD_1;
  }

  id = v < 0 ? -v : v;

  if (id > BDD_TERMID) {		/* Exceeding variable id limit. */
    fprintf (stderr, "[bdd_create_var]: Var id %d is too large.\n",
	     id);
    exit (1);
  }

  /* Get the BDD for this variable: */
  return bdd_create_node (id, BDD_1, BDD_0);
}

#ifdef COMMENT
/* Changes existing BDD variable (varid) rank value.
   Assumes that no BDD exists that refers to this variable except
   for direct references in projection functions.
   Disregards any existing grouping.
*/
void set_var_rank (int varid, int rank)
{
  int orig_rank = unique_table.ranks[varid];
  int i;

  if (orig_rank == rank)
    return;

  if (orig_rank < rank) {
    for (i = 0; i <= CURR_MAXRANK; i++) {
      int rank_i = unique_table.ranks[i];

      if (rank_i > orig_rank && rank_i <= rank)
	unique_table.ranks[i]--;
    }
  }
  else {
    /* orig_rank > rank */
    for (i = 0; i <= CURR_MAXRANK; i++) {
      int rank_i = unique_table.ranks[i];

      if (rank_i >= rank && rank_i < orig_rank)
	unique_table.ranks[i]++;
    }
  }
  unique_table.ranks[varid] = rank;
}
#endif

/* Creates a BDD for a variable with a rank number that directly follows
   the one corresponding to the argument.
   If BDD_VOID_P (v) then creates new variable that comes last in order;
   if BDD_TERM_P (v) then creates new variable that comes first in order.
*/
BDDPTR bdd_create_var_after (BDDPTR v)
{
  int nr_vars_in_use = unique_table.count;
  int index = unique_table.count;
  V_HASHTAB tab;

  /* Variable that is last in order has rank equal to count-1. */

  tab = bdd_make_fit_unique_table (index);
  /* unique_table.count has been incremented. */

  if (BDD_VOID_P (v) || nr_vars_in_use == 0)
    /* Insert as last in order: */
    /* No adjustment to ranks of existing variables necessary. */
    /* New var will be in a group by itself; no adjustments to the existing
       groups necessary.
    */
    ;
  else {
    int i, g;

    if (BDD_TERM_P (v)) {
      /* Insert as first in order: */
      /* Add variable having rank 0 (smallest rank nr. possible): */

      /* Must shift all existing variables, except last one, 1 up in rank: */
      for (i = 0; i < CURR_MAXRANK; i++)
	unique_table.ranks[i]++;
      /* i = CURR_MAXRANK */
      unique_table.ranks[i] = 0;

      /* Must reflect change of ranks in var hashtables: */
      for (/* i = CURR_MAXRANK */; i > 0; i--)
	unique_table.space[i] = unique_table.space[i-1];
      /* i = 0 */
      unique_table.space[0] = tab;

      for (g = NR_GROUPS - 1; g > 0; g--) {
	unique_table.groups[g] = unique_table.groups[g-1];
	GROUP_LAST_RANK(g)++;
      }
      GROUP_ORDERABLE(0) = 0;
      GROUP_LAST_RANK(0) = 0;
    }
    else {
      /* Insert directly following last var in group of v: */
      int group = bdd_var_id_to_group (BDD_VARID (v));
      /* rank of new variable: */
      int rank = GROUP_LAST_RANK(group) + 1;

      if (rank == CURR_MAXRANK) {
	/* Insert as last in order: */
	/* No adjustment to ranks of existing variables necessary. */
	/* New var will be in a group by itself; no adjustments to the existing
	   groups necessary.
	*/
	;
      }
      else {
	int g;

	/* Must shift the higher ranked variables 1 up in rank: */
	for (i = 0; i < CURR_MAXRANK; i++)
	  if (unique_table.ranks[i] >= rank)
	    unique_table.ranks[i]++;
	/* i = CURR_MAXRANK */
	unique_table.ranks[i] = rank;

	for (/* i = CURR_MAXRANK */; i > rank; i--)
	  unique_table.space[i] = unique_table.space[i-1];
	/* i = rank */
	unique_table.space[i] = tab;

	for (g = NR_GROUPS - 1; g > group + 1; g--) {
	  unique_table.groups[g] = unique_table.groups[g-1];
	  GROUP_LAST_RANK(g)++;
	}
	GROUP_ORDERABLE(g) = 0;
	GROUP_LAST_RANK(g) = rank;
      }
    }
  }
  return bdd_create_node (index /*non-neg*/, BDD_1, BDD_0);
}

/* Creates a BDD for a variable with a rank number that directly precedes
   the one corresponding to the argument.
   If BDD_VOID_P (v) then creates new variable that comes last in order;
   if BDD_TERM_P (v) then creates new variable that comes first in order.
*/
BDDPTR bdd_create_var_before (BDDPTR v)
{
  if (BDD_VOID_P (v) || BDD_TERM_P (v))
    return bdd_create_var_after (v);

  {
    int rank = BDD_RANK (v);
    BDDPTR before_v;
    BDDPTR result;

    if (rank == 0)
      return bdd_create_var_after (BDD_0);

    rank--;
    before_v = bdd_create_node (bdd_var_rank_to_id (rank), BDD_1, BDD_0);

    result = bdd_create_var_after (before_v);

    bdd_free (before_v);
    return result;
  }
}

BDDPTR bdd_create_var_first (void)
{
  return bdd_create_var_after (BDD_0);
}

BDDPTR bdd_create_var_last (void)
{
  return bdd_create_var_after (BDD_VOID);
}

/* Shannon expansion (v, w are real rank numbers): */
/* Case F is positive: */
#define EXP_POS_F(F, v, w)	((v < w) \
				 ? F \
				 : (BDD_I_INV_EDGE_P (F) \
				    ? BDD_ELSE (F) \
				    : BDD_THEN (F)))

#define EXP_NEG_F(F, v, w)	((v < w) \
				 ? F \
				 : (BDD_I_INV_EDGE_P (F) \
				    ? BDD_THEN (F) \
				    : BDD_ELSE (F)))

/* Case F could be negative: */
#define EXP_POS2_F(F, v, w)	((v < w) \
				 ? F \
				 : (BDD_I_INV_EDGE_P (F) \
				    ? (BDD_NEG_P (F) \
				       ? BDD_COMPL (BDD_ELSE (F)) \
				       : BDD_ELSE (F)) \
				    : (BDD_NEG_P (F) \
				       ? BDD_COMPL (BDD_THEN (F)) \
				       : BDD_THEN (F))))

#define EXP_NEG2_F(F, v, w)	((v < w) \
				 ? F \
				 : (BDD_I_INV_EDGE_P (F) \
				    ? (BDD_NEG_P (F) \
				       ? BDD_COMPL (BDD_THEN (F)) \
				       : BDD_THEN (F)) \
				    : (BDD_NEG_P (F) \
				       ? BDD_COMPL (BDD_ELSE (F)) \
				       : BDD_ELSE (F))))

#define IS_SMALLER_F(f1, f2)	(BDD_RANK (f1) < BDD_RANK (f2) \
				 ? 1 \
				 : (BDD_RANK (f1) == BDD_RANK (f2) \
				    ? f1 < f2 \
				    : 0))

/* Compute ITE (F, G, H).
   It is assumed that all arguments are ALIVE.
   The BDD returned is guaranteed to be protected.
*/
static BDDPTR bdd_ite_aux (BDDPTR F, BDDPTR G, BDDPTR H)
{
  BDDPTR R;
  int negate_result = 0;

  /* Accept BDD_VOID arguments: result will always be BDD_VOID too: */
/* For speed considerations, only top-level call bdd_ite will check this!
  if (BDD_VOID_P (F) || BDD_VOID_P (G) || BDD_VOID_P (H))
    return BDD_VOID;
*/

/* Uncomment this in case of suspicious behaviour.
  bdd_check_valid (F, "(bdd_ite_aux) F");
  bdd_check_valid (G, "(bdd_ite_aux) G");
  bdd_check_valid (H, "(bdd_ite_aux) H");
*/
  /* Quick return for constant F's: */
  if (BDD_0_P (F)) {		/* bdd_ite_aux(F=0,G,H) = H */
    BDD_INCR_REF (H);
    return H;
  }

  if (BDD_1_P  (F)) {		/* bdd_ite_aux(F=1,G,H) = G */
    BDD_INCR_REF (G);
    return G;
  }

  /* Introduce constants where possible: */
  /* Note: F not a 0 or 1! F could be X though. */

  /* (F,G=F,H) => (F,G=1,H) */
  if (BDD_EQUAL_P (F, G)) {
    /* (X,G=X,H) => X. Also covers bdd_ite_aux(X,X,X)=X */
    if (BDD_X_P (F)) {
      BDD_INCR_REF (F);
      return F;
    }
    G = BDD_1;
  }
  else
  /* (F,G=F',H) => (F,G=0,H) */
  if (BDD_COMPL_P (F, G))	/* does not apply for F=X */
    G = BDD_0;

  /* (F,G,H=F) => (F,G,H=0) */
  if (BDD_EQUAL_P (F, H)) {
    /* (X,G,H=X) => X */
    if (BDD_X_P (F)) {
      BDD_INCR_REF (F);
      return F;
    }
    H = BDD_0;
  }
  else
  /* (F,G,H=F') => (F,G,H=1) */
  if (BDD_COMPL_P (F, H))	/* does not apply for F=X */
    H = BDD_1;

  /* Here: still F not 0 or 1, perhaps G and/or H constant. */

  /* Check for trivial cases: */

  /* (F,G=1,H=0) = F. Also covers (F=X,1,0) */
  if (BDD_1_P (G) && BDD_0_P (H)) {
    BDD_INCR_REF (F);
    return F;
  }

  /* (F,G=0,H=1) = F' */
  if (BDD_0_P (G) && BDD_1_P (H)) {
    /* Mind that if F=X we should NOT complement! */
    if (BDD_X_P (F)) {
      BDD_INCR_REF (F);
      return F;
    }

    /* Can only quickly complement when using complemented edges. */
    if (bdd_use_neg_edges) {
      BDD_INCR_REF (F);
      return BDD_COMPL (F);
    }
  }

  /* (F,G,H=G) = G */
  if (BDD_EQUAL_P (G, H)) {
    BDD_INCR_REF (G);
    return G;
  }

  /* Here: F not 0 or 1; G, H different. */
  /* NOTE: When !bdd_use_neg_edges perhaps G=0, H=1 */

  /* Make into standard triples: */
  /* For first arg choose the one with `smallest' top variable. */

  /* (F,1,H) => (H,1,F) */
  if (BDD_1_P (G))
    if (IS_SMALLER_F (H, F)) {
      R = H;
      H = F;
      F = R;
    }

  /* Here: F non-constant; G, H different. */

  /* (F,G,0) => (G,F,0) */
  if (BDD_0_P (H))
    if (IS_SMALLER_F (G, F)) {
      R = G;
      G = F;
      F = R;
    }

  /* Here: F non-constant; G, H different. */

  if (bdd_use_neg_edges) {

  /* (F,G,1) => (G',F',1) */
  if (BDD_1_P (H))
    if (IS_SMALLER_F (G, F)) {
      R = G;
      /* Here: G is not a constant. */
      G = BDD_COMPL (F);
      F = BDD_COMPL (R);
    }

  /* Here: F non-constant, perhaps X; G, H different. */

  /* (F,0,H) => (H',0,F') */
  if (BDD_0_P (G))
    if (IS_SMALLER_F (H, F)) {
      R = H;
      /* Here: H is not a constant. */
      H = BDD_COMPL (F);
      F = BDD_COMPL (R);
    }

  /* Here: F non-constant; G, H different. */

  /* (F,G,G') => (G,F,F') */
  if (BDD_COMPL_P (G, H)) {
    if (BDD_X_P (F)) {
      BDD_INCR_REF (F);
      return F;
    }

    if (IS_SMALLER_F (G, F)) {

      /* Here: G is not a constant. */
      H = F;
      /* (F, G, F) */
      F = G;
      /* (G, G, F) */
      G = H;
      /* (G, F, F) */
      H = BDD_COMPL (H);
      /* (G, F, F') */
    }
  }

  /* Here: F non-constant; G, H different. */

  /* First and second arg should not be a complemented (= negative) edge.
     Use: (F,G,H) = (F',H,G) = (F,G',H')' = (F',H',G')'
  */

  /* (F',G,H) => (F,H,G) */
  if (BDD_NEG_P (F)) {
    R = H;
    /* Surely F not X since X's cannot have complemented bit set. */
    F = BDD_O_OFF (F);
    H = G;
    G = R;
  }

  /* Here: F non-constant and positive;
     G, H different.
  */

  /* (F,G',H) => (F,G,H')' */
  if (BDD_NEG_P (G)) {
    negate_result = 1;
    G = BDD_O_OFF (G);
    H = BDD_COMPL (H);
  }

  } /*if (bdd_use_neg_edges)...*/

  /* Here: F non-constant and positive,
     G also positive,
     G, H different.
  */
  /* NOTE: When !bdd_use_neg_edges perhaps G=0, H=1 */

  if (!BDD_VOID_P (R = bdd_lookup_computed_table (F, G, H))) {
    BDD_INCR_REF (R);
    return negate_result ? BDD_COMPL (R) : R;
  }

  /* bdd_ite_aux(F=v['],G,H) = (v['],G,H) if v < top(G,H) */
  if (BDD_1_P (BDD_THEN (F)) && BDD_0_P (BDD_ELSE (F))) {
    int v = BDD_RANK (F);

    if (v < BDD_RANK (G) && v < BDD_RANK (H)) {
      if (BDD_I_INV_EDGE_P (F))
	R = bdd_create_node (BDD_VARID (F) /*non-neg*/, H, G);
      else
	R = bdd_create_node (BDD_VARID (F) /*non-neg*/, G, H);

      bdd_insert_computed_table (F, G, H, R);

      return negate_result ? BDD_COMPL (R) : R;
    }
  }

  {
    BDDPTR T, E;
    BDDPTR Fv_not, Gv_not, Hv_not;
    int topF  = BDD_RANK (F);	/* F is positive */
    int topG  = BDD_RANK (G);	/* G is positive */
    int topH  = BDD_RANK (H);
    int varid;
    int rank;

    /* Watch out! Ranks for variables might change because of
       dynamic variable ordering during bdd_ite_aux.

       Currently it is decided that dynamic variable ordering
       never appears during bdd_ite_aux. The extra precautions here
       don't add much to the overall run-time however.
       Still we don't do them!
    */

    /* Get the smallest of the three rank numbers: */
    if (topG < topH) {
      varid = BDD_VARID (G);
      rank = topG;
    }
    else {
      varid = BDD_VARID (H);
      rank = topH;
    }

    if (topF < rank) {
      varid = BDD_VARID (F);
      rank = topF;
    }

    /* Note: F,G,H can have input-inverted bit on! */

    Fv_not = EXP_NEG_F  (F, rank, topF);
    Gv_not = EXP_NEG_F  (G, rank, topG);
    Hv_not = EXP_NEG2_F (H, rank, topH);

    /* Make sure the negative cofactors stay alive: */
/* Only when allowing DVO during bdd_ite_aux.
    BDD_INCR_REF (Fv_not);
    BDD_INCR_REF (Gv_not);
    BDD_INCR_REF (Hv_not);
*/

    T = bdd_ite_aux (EXP_POS_F  (F, rank, topF),
		     EXP_POS_F  (G, rank, topG),
		     EXP_POS2_F (H, rank, topH));

    /* Note: T is protected from garbage collection: */

    if (BDD_VOID_P (T)) {
/* Only when allowing DVO during bdd_ite_aux.
      bdd_free (Fv_not);
      bdd_free (Gv_not);
      bdd_free (Hv_not);
*/
      return BDD_VOID;
    }

    E = bdd_ite_aux (Fv_not, Gv_not, Hv_not);

/* Only when allowing DVO during bdd_ite_aux.
    bdd_free (Fv_not);
    bdd_free (Gv_not);
    bdd_free (Hv_not);
*/
    if (BDD_VOID_P (E))
      return BDD_VOID;

    R = bdd_create_node (varid /*non-neg*/, T, E);
    bdd_free (T);
    bdd_free (E);

    bdd_insert_computed_table (F, G, H, R);

    return negate_result ? BDD_COMPL (R) : R;
  }
}

#define bdd_lookup_apply_cache(f, a, b) \
	bdd_lookup_computed_table (BDD_F_SET(f),(a),(b))

#define bdd_insert_apply_cache(f, a, b, R) \
	bdd_insert_computed_table (BDD_F_SET(f),(a),(b),(R))

/* Applies function f to BDDs for a and b. Result is BDD for f(a,b).
   Arguments in actual calls to f will be terminal nodes.
*/
static BDDPTR (*func)(BDDPTR,BDDPTR);

static BDDPTR bdd_apply_aux (BDDPTR a, BDDPTR b)
{
  int top_a, top_b, rank, varid;
  BDDPTR T, E, R;

  if (BDD_VOID_P (a) || BDD_VOID_P (b))
    return BDD_VOID;
/*
  bdd_check_valid (a, "(bdd_apply) a");
  bdd_check_valid (b, "(bdd_apply) b");
*/

  if (BDD_TERM_P (a) && BDD_TERM_P (b))
    return func (a, b);

  if (!BDD_VOID_P (R = bdd_lookup_apply_cache (func, a, b)))
    return bdd_assign (R);

  /* Get the smallest of the two rank numbers: */
  top_a = BDD_RANK (a);
  top_b = BDD_RANK (b);

  if (top_a < top_b) {
    varid = BDD_VARID (a);
    rank = top_a;
  }
  else {
    varid = BDD_VARID (b);
    rank = top_b;
  }

  T = bdd_apply_aux (EXP_POS2_F (a, rank, top_a), EXP_POS2_F (b, rank, top_b));

  if (BDD_VOID_P (T)) return T;

  E = bdd_apply_aux (EXP_NEG2_F (a, rank, top_a), EXP_NEG2_F (b, rank, top_b));

  if (BDD_VOID_P (E)) {
    bdd_free (T);
    return E;
  }

  R = bdd_create_node (varid, T, E);
  bdd_free (T);
  bdd_free (E);

  return bdd_insert_apply_cache (func, a, b, R);
}

BDDPTR bdd_apply (BDDPTR (*f)(BDDPTR,BDDPTR), BDDPTR a, BDDPTR b)
{
  BDDPTR (*save_func)(BDDPTR,BDDPTR);
  BDDPTR R;

  if (!f || BDD_VOID_P (a) || BDD_VOID_P (b))
    return BDD_VOID;

/*
   This test is not necessary because function as stored in entry->F of
   computed_table is never retrieved.

  if (BDD_FUNC_P (f)) {
    fprintf (stderr, "[bdd_apply]: Address of function must have 0 MSB.\n");
    exit (1);
  }
*/
  /* Allow for function f itself to call bdd_apply (i.e., stack 'em): */
  save_func = func;
  func = f;
  R = bdd_apply_aux (a, b);
  func = save_func;
  return R;
}

#if 0

/* Test of bdd_apply */

static BDDPTR plus (BDDPTR a, BDDPTR b)
{
  long val_a, val_b;

  if (BDD_X_P (a) || BDD_X_P (b))
    return bdd_X ();

  if (BDD_0_P (a))
    return bdd_assign (b);

  if (BDD_0_P (b))
    return bdd_assign (a);

  val_a = BDD_1_P (a) ? 1 : (long) BDD_USER_TERM_DATA (a);
  val_b = BDD_1_P (b) ? 1 : (long) BDD_USER_TERM_DATA (b);

  return bdd_create_user_terminal ((void *) (val_a + val_b));
}

BDDPTR bdd_plus (BDDPTR a, BDDPTR b)
{
  return bdd_apply (plus, a, b);
}

static BDDPTR or (BDDPTR a, BDDPTR b)
{
  return bdd_assign (BDD_BOOL (BDD_TRUE_P (a) || BDD_TRUE_P (b)));
}

static BDDPTR and (BDDPTR a, BDDPTR b)
{
  return bdd_assign (BDD_BOOL (BDD_TRUE_P (a) && BDD_TRUE_P (b)));
}

static BDDPTR xor (BDDPTR a, BDDPTR b)
{
  return bdd_assign (BDD_BOOL (   BDD_FALSE_P (a) && BDD_TRUE_P (b)
			       || BDD_TRUE_P  (a) && BDD_FALSE_P (b)));
}

/* Emulates bdd_ite_aux using several calls to bdd_apply.
   This seems to be almost as fast as the real bdd_ite_aux.
*/
static BDDPTR bdd_ite_aux (BDDPTR F, BDDPTR G, BDDPTR H)
{
  BDDPTR R1    = bdd_apply (and, F, G);
  BDDPTR F_not = bdd_apply (xor, F, BDD_1);
  BDDPTR R2    = bdd_apply (and, F_not, H);
  BDDPTR R     = bdd_apply (or, R1, R2);

  bdd_free (F_not);
  bdd_free (R1);
  bdd_free (R2);
  return R;
}
#endif

#define bdd_dyna_threshold_default	4096
int bdd_dyna_monitor = 0;
float bdd_dyna_threshold_factor = 1.25 /*2.0*/;

BDDPTR bdd_ite (BDDPTR F, BDDPTR G, BDDPTR H)
{
  BDDPTR R;
  static FILE *fp = NULL;
  static int history;
  static int bdd_dyna_threshold;
  int alive;
  int reason;
  float increase;

  /* Accept BDD_VOID arguments: result will always be BDD_VOID too: */
  if (BDD_VOID_P (F) || BDD_VOID_P (G) || BDD_VOID_P (H))
    return BDD_VOID;

/* Uncomment this in case of suspicious behaviour.
  bdd_check_valid (F, "(bdd_ite) F");
  bdd_check_valid (G, "(bdd_ite) G");
  bdd_check_valid (H, "(bdd_ite) H");
*/
  if (!fp) {
    if (bdd_dyna_monitor) {
      fp = fopen ("./ITE_prof", "w");
      fprintf (fp, "1 BDD_Nodes_Alive\n");
      fprintf (fp, "2 Increase\n");
      fprintf (fp, "-1\n");
    }
    else
      fp = (FILE *) 1;

    history = unique_table.nr_items - bdd_nr_dead_nodes;
    bdd_dyna_threshold = bdd_dyna_threshold_default;

#if defined(BDD_DEBUG)
    fprintf (stderr, "*** Absolute threshold set to %d alive nodes.\n",
	     bdd_dyna_threshold);
#endif
  }

  R = bdd_ite_aux (F, G, H);
  bdd_nr_ite_calls++;
  alive = unique_table.nr_items - bdd_nr_dead_nodes;

  if (history >= 0) {
  increase = ((float) alive) / ++history;

  if (bdd_dyna_monitor) {
    fprintf (fp, "1 %d %d\n", bdd_nr_ite_calls, alive);
    fprintf (fp, "2 %d %f\n", bdd_nr_ite_calls, increase);
    fflush (fp);
  }

  if (increase < 0.1) {
    /* Nr. nodes decreases dramatically. */

    /* Reset threshold but don't go below nr. alive: */
    bdd_dyna_threshold = max (bdd_dyna_threshold_default, alive << 1);

#if defined(BDD_DEBUG)
    fprintf (stderr, "*** Absolute threshold set to %d alive nodes.\n",
	     bdd_dyna_threshold);
#endif
  }

  if (   (reason = 1, increase > 1.9)
      || (reason = 2, alive > bdd_dyna_threshold)) {
    if (bdd_do_dynamic_ordering && alive > bdd_nr_vars) {
      int gain;

      if (bdd_verbose)
	fprintf (stderr,
	 "*** %d Dynamic variable ordering at ite call %d; Reason %d.\n",
		 bdd_nr_dynamic+1, bdd_nr_ite_calls, reason);

      gain = bdd_dynamic_order ();

      /* Could use success rate to adjust trigger criteria. */
/*      if (((float) gain) < 0.9 * alive) {*/
	/* Not able to reduce to less than 10%. */
	if (reason == 2) {
	  /* Koen's heuristic: */
	  float rel_shr = 1.0 - ((float) gain) / ((float) alive);

	  bdd_dyna_threshold = unique_table.nr_items
	                       * (1.0 + rel_shr * rel_shr);

	  /* Rudell's/Kostelijk's heuristic: */
/*	  bdd_dyna_threshold <<= 1;*/

#if defined(BDD_DEBUG)
	  fprintf (stderr, "*** Absolute threshold set to %d alive nodes.\n",
		   bdd_dyna_threshold);
#endif
	}
/*      }*/
    }
  }
  }
  history = alive;

  return R;
}

#define	INT(F)		BDD_TERM_P (F) ? F : BDD_VOID

/* Compute ITE_CONST (F, G, H), i.e. the same actions as ITE except
   that as soon as it is determined that the result is non-constant
   processing stops.
   It is assumed that all arguments are ALIVE.
   Either BDD_0, BDD_1, BDD_X, or BDD_VOID is returned.
   The result BDD_VOID is to be interpreted as NOT-CONSTANT.
*/
BDDPTR bdd_ite_const (BDDPTR F, BDDPTR G, BDDPTR H)
{
  BDDPTR R;
  int negate_result = 0;

  if (BDD_VOID_P (F) || BDD_VOID_P (G) || BDD_VOID_P (H))
    return BDD_VOID;
/*
  bdd_check_valid (F, "(bdd_ite_const) F");
  bdd_check_valid (G, "(bdd_ite_const) G");
  bdd_check_valid (H, "(bdd_ite_const) H");
*/
  /* Quick return for constant F's: */
  if (BDD_0_P (F)) return INT (H);	/* bdd_ite(F=0,G,H) = H */

  if (BDD_1_P (F))  return INT (G);	/* bdd_ite(F=1,G,H) = G */

  /* Introduce constants where possible: */
  /* Note: F not a 0 or 1! F could be X though. */

  /* (F,G=F,H) => (F,G=1,H) */
  if (BDD_EQUAL_P (F, G)) {
    /* (X,G=X,H) => X. Also covers bdd_ite(X,X,X)=X */
    if (BDD_X_P (F)) return F;
    G = BDD_1;
  }
  else
  /* (F,G=F',H) => (F,G=0,H) */
  if (BDD_COMPL_P (F, G)) G = BDD_0; /* does not apply for F=X */

  /* (F,G,H=F) => (F,G,H=0) */
  if (BDD_EQUAL_P (F, H)) {
    /* (X,G,H=X) => X */
    if (BDD_X_P (F)) return F;
    H = BDD_0;
  }
  else
  /* (F,G,H=F') => (F,G,H=1) */
  if (BDD_COMPL_P (F, H)) H = BDD_1; /* does not apply for F=X */

  /* Here: still F not 0 or 1, perhaps G and/or H constant. */

  /* Check for trivial cases: */

  /* (F,G=1,H=0) = F. Also covers (F=X,1,0) */
  if (BDD_1_P (G) && BDD_0_P (H))
    return INT (F);

  /* (F,G=0,H=1) = F' */
  if (BDD_0_P (G) && BDD_1_P (H)) {
    /* Mind that if F=X we should NOT complement! */
    if (BDD_X_P (F)) return F;

    if (bdd_use_neg_edges) return INT (BDD_COMPL (F));
  }

  /* (F,G,H=G) = G */
  if (BDD_EQUAL_P (G, H))
    return INT (G);

  /* Here: F not 0 or 1; G, H different. */
  /* NOTE: When !bdd_use_neg_edges perhaps G=0, H=1 */

  /* Make into standard triples: */
  /* For first arg choose `smallest' top variable. */

  /* (F,1,H) => (H,1,F) */
  if (BDD_1_P (G))
    if (IS_SMALLER_F (H, F)) {
      R = H;
      H = F;
      F = R;
    }

  /* Here: F non-constant; G, H different. */

  /* (F,G,0) => (G,F,0) */
  if (BDD_0_P (H))
    if (IS_SMALLER_F (G, F)) {
      R = G;
      G = F;
      F = R;
    }

  /* Here: F non-constant; G, H different. */

  if (bdd_use_neg_edges) {

  /* (F,G,1) => (G',F',1) */
  if (BDD_1_P (H))
    if (IS_SMALLER_F (G, F)) {
      R = G;
      /* Here: G is not a constant. */
      G = BDD_COMPL (F);
      F = BDD_COMPL (R);
    }

  /* Here: F non-constant; G, H different. */

  /* (F,0,H) => (H',0,F') */
  if (BDD_0_P (G))
    if (IS_SMALLER_F (H, F)) {
      R = H;
      /* Here: H is not a constant. */
      H = BDD_COMPL (F);
      F = BDD_COMPL (R);
    }

  /* Here: F non-constant; G, H different. */

  /* (F,G,G') => (G,F,F') */
  if (BDD_COMPL_P (G, H)) {
    if (BDD_X_P (F)) return F;

    if (IS_SMALLER_F (G, F)) {

      /* Here: G is not a constant. */
      H = F;
      /* (F, G, F) */
      F = G;
      /* (G, G, F) */
      G = H;
      /* (G, F, F) */
      H = BDD_COMPL (H);
      /* (G, F, F') */
    }
  }
  /* Here: F non-constant; G, H different. */

  /* First and second arg should not be a complemented (= negative) edge.
     Use: (F,G,H) = (F',H,G) = (F,G',H')' = (F',H',G')'
  */

  /* (F',G,H) => (F,H,G) */
  if (BDD_NEG_P (F)) {
    R = H;
    F = BDD_O_OFF (F);
    H = G;
    G = R;
  }

  /* Here: F non-constant and positive;
     G, H different.
  */

  /* (F,G',H) => (F,G,H')' */
  if (BDD_NEG_P (G)) {
    negate_result = 1;
    G = BDD_O_OFF (G);
    H = BDD_COMPL (H);
  }

  } /*if (bdd_use_neg_edges)...*/

  /* Here: F non-constant and positive,
     G also positive,
     G, H different.
  */
  /* NOTE: When !bdd_use_neg_edges perhaps G=0, H=1 */

  if (!BDD_VOID_P (R = bdd_lookup_computed_table_no_reclaim (F, G, H))) {
    if (negate_result) R = BDD_COMPL (R);
    return INT (R);
  }

  {
    BDDPTR T, E;
    int topF  = BDD_RANK (F);	/* F is positive */
    int topG  = BDD_RANK (G);	/* G is positive */
    int topH  = BDD_RANK (H);
    int minGH = topG < topH  ? topG : topH;
    int rank  = topF < minGH ? topF : minGH;

    /* Watch out! Ranks for variables might change because of
       dynamic variable ordering.
       However, never occurs during bdd_ite_const.
    */

    /* Note: F,G,H can have input-inverted bit on! */

    T = bdd_ite_const (EXP_POS_F  (F, rank, topF),
		       EXP_POS_F  (G, rank, topG),
		       EXP_POS2_F (H, rank, topH));

    if (BDD_VOID_P (T))
      return T;

    /* Here T is a constant; no need to protect from GC. */
    E = bdd_ite_const (EXP_NEG_F  (F, rank, topF),
		       EXP_NEG_F  (G, rank, topG),
		       EXP_NEG2_F (H, rank, topH));

    if (!BDD_EQUAL_P (T, E))
      return BDD_VOID;

    /* Here T is a constant. */

    bdd_insert_computed_table (F, G, H, T);

    return negate_result ? BDD_COMPL (T) : T;
  }
}
#undef INT

void bdd_cofactors (BDDPTR f, BDDPTR *vp, BDDPTR *Tp, BDDPTR *Ep)
{
  BDDPTR v, T, E;

  if (BDD_VOID_P (f))
    v = T = E = BDD_VOID;
  else
  if (BDD_TERM_P (f)) {
    v = f;
    BDD_INCR_REF (v);
    T = BDD_1;
    E = BDD_0;
  }
  else {
    v = bdd_create_var (BDD_VARID (f));
    T = BDD_COFACTOR_POS (f);
    E = BDD_COFACTOR_NEG (f);
  }
  BDD_INCR_REF (T);
  BDD_INCR_REF (E);
  *vp = v;
  *Tp = T;
  *Ep = E;
}

/* Returns f with its top-variable inverted,
   i.e. say f = v.T + v'.E then result is v'.T + v.E.
   Returns f without change in case it is BDD_VOID or a constant.
   Result is protected.
*/
BDDPTR bdd_invert_input_top (BDDPTR f)
{
  BDDPTR v, T, E;

  /* Cannot just flip the inverted-input bit since not sure
     whether T < E holds.
  */

  bdd_cofactors (f, &v, &T, &E);
  f = bdd_ite (v, E, T);
  bdd_free (v);
  bdd_free (T);
  bdd_free (E);
  return f;
}

static FILE *global_fp;

static void bdd_print_node_aux (BDDPTR v)
{
  if (BDD_VOID_P (v)) {
    fprintf (global_fp, "'-', [   ], &v: 0x00000000\n");
    return;
  }

  if (BDD_0_P (v))
    fprintf (global_fp, "'0', [ oo], ");
  else
  if (BDD_1_P (v))
    fprintf (global_fp, "'1', [ oo], ");
  else
  if (BDD_X_P (v))
    fprintf (global_fp, "'X', [ oo], ");
  else
  if (BDD_TERM_P (v))
    /* User introduced special terminal node. */
    fprintf (global_fp, "'S', [ oo], ");
  else
    fprintf (global_fp, "%3d, [%3d], ", BDD_VARID (v), BDD_RANK (v));
  fprintf (global_fp, "(M:%s), ", BDD_MARK (v) ? "1" : "0");

  fprintf (global_fp, "&v: %p, Refs: %3d, Then: %p, Else: %p\n",
	   v,
	   BDD_REFCOUNT (v),
	   BDD_THEN (v),
	   BDD_ELSE (v));
}

void bdd_print_node (FILE *fp, BDDPTR v)
{
  global_fp = fp;
  bdd_print_node_aux (v);
}

void bdd_print (FILE *fp, BDDPTR f, char *s)
{
  fprintf (fp, "BDD: %s\n", s);
  if (BDD_VOID_P (f))
    fprintf (fp, "void\n");
  else {
    global_fp = fp;
    bdd_traverse_pre (f, bdd_print_node_aux);
    bdd_reset_marks (f);
  }
}

static void print_computed_table_stats (FILE *fp)
{
  int size          = computed_table ? BDD_CT_SIZE   (computed_table) : 0;
  int nr_hits       = computed_table ? BDD_CT_HITS   (computed_table) : 0;
  int nr_lookups    = computed_table ? BDD_CT_LOOKUPS(computed_table) : 0;
  int nr_collisions = computed_table ? BDD_CT_COLLS  (computed_table) : 0;
  int nr_items      = computed_table ? BDD_CT_ITEMS  (computed_table) : 0;

  fprintf (fp, "*** BDD Computed Table Cache Info ***\n");
  fprintf (fp,
	   "%d lookups, %d hits, %d%% success (%d collisions, %d%% occ).\n",
	   nr_lookups, nr_hits,
	   nr_lookups ? nr_hits * 100 / nr_lookups : 100,
	   nr_collisions,
	   size ? (nr_items * 100) / size : 0);
}

#if 0
static void bdd_print_var_order (FILE *fp)
{
  int i;

  fprintf (fp, "*** Variable id's in use and their rank ***\n");

  for (i = 0; i < unique_table.count; i++)
    fprintf (fp, "id: %d, rank: %d\n", i, unique_table.ranks[i]);
}
#endif

void bdd_print_stats (FILE *fp)
{
/*  bdd_print_var_order (fp);*/
  print_unique_table_stats (fp);
  print_computed_table_stats (fp);
}

void bdd_init (void)
{
  /* Guard against multiple initialisations: */
  if (BDD_PACKAGE_INITIALIZED) {
    if (bdd_verbose)
      fprintf (stderr, "[bdd_init]: Package already initialized.\n");
    return;
  }

  if (bdd_verbose) {
    fprintf (stderr,
"[bdd_init]: v1.8, November 26 1996, Copyright (C) 1996 G. Janssen\n"
"[bdd_init]: Technical University Eindhoven, Netherlands\n");

    fprintf (stderr, "[bdd_init]: BDD_MAXVARS == %d.\n", BDD_MAXVARS);
    fprintf (stderr, "[bdd_init]: BDD_MAXREFCOUNT == %d.\n", BDD_MAXREFCOUNT);
    fprintf (stderr, "[bdd_init]: Memory limit %d kb.\n",
	     bdd_allowed_memsize >> 10);
    fprintf (stderr, "[bdd_init]: bdd_use_neg_edges == %d.\n",
	     bdd_use_neg_edges);
    fprintf (stderr, "[bdd_init]: bdd_do_dynamic_ordering == %d.\n",
	     bdd_do_dynamic_ordering);
    fprintf (stderr, "[bdd_init]: bdd_do_gc == %d.\n",
	     bdd_do_gc);
  }

  if (bdd_do_dynamic_ordering) {
    bdd_use_inv_edges = 0;
    if (bdd_verbose)
      fprintf (stderr, 
"[bdd_init]: bdd_use_inv_edges == 0 (Because of dynamic variable ordering).\n");
  }
  else
    if (bdd_verbose)
      fprintf (stderr, 
"[bdd_init]: bdd_use_inv_edges == %d.\n", bdd_use_inv_edges);

  /* Make sure that BDDPTRs are aligned on word boundary:
     This means that bdd_sizeof_user_data must be evenly dividable by 4.
          &3? &~3 +4
     0 -> 0 0 0
     1 -> 1 0 4
     2 -> 1 0 4
     3 -> 1 0 4
     4 -> 0 4 4
     5 -> 1 4 8
     6 -> 1 4 8
     7 -> 1 4 8
     8 -> 0 8 8
     9 -> 1 8 12
  */
  if (bdd_sizeof_user_data & 3) {
    bdd_sizeof_user_data &= ~3;
    bdd_sizeof_user_data += 4;
  }
  bdd_sizeof = sizeof (struct bdd) + bdd_sizeof_user_data;

  if (bdd_verbose)
    fprintf (stderr,
"[bdd_init]: %d bytes in 1 BDD node (%d including overhead).\n",
	     bdd_sizeof, bdd_sizeof + 6);

  nr_nodes_per_block =
    (BDD_BLK_SIZEOF - sizeof (BLKPTR) /*next field*/) / bdd_sizeof;

  BDD_X = BDD_CALLOC ();
  bdd_nr_const_nodes++;
  BDD_VARID (BDD_X) = BDD_TERMID;
/*  BDD_INCR_REF (BDD_X);*/

  /* Using inverted input edges requires that BDD_1 < BDD_0, so that
     the BDD <v, 1, 0> appears with no inv bit set.
     Therefore here we make sure this invariant holds.
  */
  BDD_1 = BDD_CALLOC ();
  bdd_nr_const_nodes++;
  BDD_VARID (BDD_1) = BDD_TERMID;
  if (bdd_use_neg_edges)
    /* The 0-function will be the complement of 1-function: */
    BDD_0 = BDD_O_SET_U (BDD_1);
  else {
    BDD_0 = BDD_CALLOC ();
    bdd_nr_const_nodes++;
    BDD_VARID (BDD_0) = BDD_TERMID;
  }
  if (BDD_1 > BDD_0) {
    BDDPTR tmp = BDD_1;

    BDD_1 = BDD_0;
    BDD_0 = tmp;
  }
/*
  BDD_INCR_REF (BDD_1);
  BDD_INCR_REF (BDD_0);
*/

  /* Note that CONSTANTS will not appear in the unique_table, so there is no
     need to protect them from garbage collection. */
  unique_table = bdd_make_unique_table ();
  bdd_make_computed_table (BDD_COMPUTED_TABLE_SIZE);

  BDD_PACKAGE_INITIALIZED = 1;
}

/* Free all memory occupied by the BDD package:
   Must again call bdd_init to continue using the package.

   In effect: block_free_list, unique_table, computed_table.
*/
void bdd_quit (void)
{
  register int i;

  /* Guard against quit without init (includes multiple quits): */
  if (!BDD_PACKAGE_INITIALIZED) {
    if (bdd_verbose)
      fprintf (stderr, "[bdd_quit]: Package not initialized.\n");
    return;
  }

  /* Must also cleanup SOP cache of bdd_fns.c: */
  bdd_cleanup_sop_cache ();

  bdd_free_blocks ();

  for (i = 0; i < unique_table.count; i++) {
    V_HASHTAB tab = unique_table.space[i];

    if (tab)
      BDD_FREE_BYTES (tab, BDD_VUT_SIZEOF (BDD_VUT_LOG2SIZE(tab)));
  }
  BDD_FREE_ARRAY (unique_table.space,  unique_table.size, V_HASHTAB);
  BDD_FREE_ARRAY (unique_table.ranks,  unique_table.size, int);
  BDD_FREE_ARRAY (unique_table.groups, unique_table.size, GROUP_REC);
  if (unique_table.terms) {
    BDD_FREE_BYTES (unique_table.terms,
		    BDD_VUT_SIZEOF (BDD_VUT_LOG2SIZE(unique_table.terms)));
    unique_table.terms = NULL;
  }

  unique_table.space      = NULL;
  unique_table.ranks      = NULL;
  unique_table.groups     = NULL;
  unique_table.size       = 0;
  unique_table.count      = 0;
  unique_table.nr_groups  = 0;
  unique_table.nr_items   = 0;
  unique_table.nr_entries = 0;

  if (computed_table) {
    BDD_FREE_BYTES (computed_table,
		    BDD_CT_SIZEOF (BDD_CT_LOG2SIZE(computed_table)));
    computed_table = NULL;
  }

  bdd_nr_const_nodes = 0;
  bdd_nr_vars        = 0;
  bdd_max_bytes_used = 0;
  bdd_peak_nr_nodes  = 0;
  bdd_peak_nr_nodes_alive = 0;
  bdd_nr_gc          = 0;
  bdd_nr_dynamic     = 0;
  bdd_nr_ite_calls   = 0;

  BDD_PACKAGE_INITIALIZED = 0;
}

/* Returns total of bytes currently in use by BDD package. */
int bdd_memsize (void)
{
  return bdd_bytes_allocated;
/*
  return   nr_blocks_allocated * BDD_BLK_SIZEOF
	 + unique_table.size * (  sizeof (V_HASHTAB)
	                        + sizeof (int)
				+ sizeof (GROUP_REC))
	 + unique_table.nr_entries * sizeof (BDDPTR)
	 + (computed_table ? BDD_CT_SIZEOF (BDD_CT_LOG2SIZE(computed_table))
	                   : 0);
*/
}

int bdd_top_memsize (void)
{
  return bdd_max_bytes_used;
}

static BDDPTR bdd_default_nodes_limit_handler (void)
{
  fprintf (stderr,
	   "[bdd]: Package Number Nodes Limit (%d) Exceeded. Exiting...\n",
	   bdd_nr_nodes_allowed);
  exit (1);
  return BDD_VOID;
}

static void bdd_default_memfull_handler (void)
{
  fprintf (stderr,
	   "[bdd]: Package Memory Limit (%d kb) Exceeded. Exiting...\n",
	   bdd_allowed_memsize >> 10);
  exit (1);
}

void bdd_set_memsize_limit_and_handler (int limit, void (*handler) (void))
{
  bdd_memfull_handler = handler ? handler : bdd_default_memfull_handler;
  bdd_allowed_memsize = limit;
}

void bdd_set_gc_hook (void (*hook) (void))
{
  bdd_gc_hook = hook;
}

int bdd_memsize_limit (void)
{
  return bdd_allowed_memsize;
}

int bdd_equal_p (BDDPTR F, BDDPTR G)
{
  return BDD_EQUAL_P (F, G);
}

int bdd_compl_p (BDDPTR F, BDDPTR G)
{
  BDDPTR not_F = BDD_NOT (F);
  int result;

  result = BDD_EQUAL_P (not_F, G);
  bdd_free (not_F);
  return result;
}

BDDPTR bdd_not (BDDPTR F)
{
  return bdd_ite (F, BDD_0, BDD_1);
}

BDDPTR bdd_and (BDDPTR F, BDDPTR G)
{
  return bdd_ite (F, G, BDD_0);
}

BDDPTR bdd_greater (BDDPTR F, BDDPTR G)
{
  return bdd_ite (G, BDD_0, F);
}

BDDPTR bdd_less (BDDPTR F, BDDPTR G)
{
  return bdd_ite (F, BDD_0, G);
}

BDDPTR bdd_xor (BDDPTR F, BDDPTR G)
{
  BDDPTR R, not_G = BDD_NOT (G);

  R = bdd_ite (F, not_G, G);
  bdd_free (not_G);
  return R;
}

BDDPTR bdd_or (BDDPTR F, BDDPTR G)
{
  return bdd_ite (F, BDD_1, G);
}

BDDPTR bdd_nor (BDDPTR F, BDDPTR G)
{
  BDDPTR R, not_G = BDD_NOT (G);

  R = bdd_ite (F, BDD_0, not_G);
  bdd_free (not_G);
  return R;
}

BDDPTR bdd_equiv (BDDPTR F, BDDPTR G)
{
  BDDPTR R, not_G = BDD_NOT (G);

  R = bdd_ite (F, G, not_G);
  bdd_free (not_G);
  return R;
}

BDDPTR bdd_xnor (BDDPTR F, BDDPTR G) /* equivalent to bdd_equiv */
{
  BDDPTR R, not_G = BDD_NOT (G);

  R = bdd_ite (F, G, not_G);
  bdd_free (not_G);
  return R;
}

BDDPTR bdd_implied (BDDPTR F, BDDPTR G)
{
  BDDPTR R, not_G = BDD_NOT (G);

  R = bdd_ite (F, BDD_1, not_G);
  bdd_free (not_G);
  return R;
}

BDDPTR bdd_implies (BDDPTR F, BDDPTR G)
{
  return bdd_ite (F, G, BDD_1);
}

int bdd_implies_taut (BDDPTR F, BDDPTR G)
{
  return BDD_IMPLIES_TAUT (F, G);
}

BDDPTR bdd_nand (BDDPTR F, BDDPTR G)
{
  BDDPTR R, not_G = BDD_NOT (G);

  R = bdd_ite (F, not_G, BDD_1);
  bdd_free (not_G);
  return R;
}

BDDPTR bdd_0 (void)
{
  BDD_INCR_REF (BDD_0);
  return BDD_0;
}

BDDPTR bdd_1 (void)
{
  BDD_INCR_REF (BDD_1);
  return BDD_1;
}

BDDPTR bdd_X (void)
{
  BDD_INCR_REF (BDD_X);
  return BDD_X;
}

BDDPTR bdd_assign (BDDPTR f)
{
  BDD_INCR_REF (f);
  return f;
}

int bdd_const_p (BDDPTR f)
{
  return BDD_VOID_P (f) ? 0 : BDD_CONST_P (f);
}

int bdd_literal_p (BDDPTR f)
{
  return BDD_VOID_P (f) ? 0 : BDD_LIT_P (f);
}

int bdd_poslit_p (BDDPTR f)
{
  return BDD_VOID_P (f) ? 0 : BDD_POSLIT_P (f);
}

int bdd_neglit_p (BDDPTR f)
{
  return BDD_VOID_P (f) ? 0 : BDD_NEGLIT_P (f);
}

int bdd_void_p (BDDPTR f)
{
  return BDD_VOID_P (f);
}

int bdd_frozen_p (BDDPTR f)
{
  return BDD_VOID_P (f) ? 0 : BDD_FROZEN_P (f);
}

BDDPTR bdd_top_var (BDDPTR f)
{
  if (BDD_VOID_P (f))
    return BDD_VOID;

  if (BDD_TERM_P (f)) {
    BDD_INCR_REF (f);
    return f;
  }
  return bdd_create_var (BDD_VARID (f));
}

int bdd_top_var_id (BDDPTR f)
{
  return BDD_VOID_P (f) ? -1 : BDD_VARID (f);
}

int bdd_top_var_rank (BDDPTR f)
{
  return BDD_VOID_P (f) ? -1 : BDD_RANK (f);
}

BDDPTR bdd_then (BDDPTR f)
{
  BDDPTR R = BDD_COFACTOR_POS (f);

  BDD_INCR_REF (R);
  return R;
}

BDDPTR bdd_else (BDDPTR f)
{
  BDDPTR R = BDD_COFACTOR_NEG (f);

  BDD_INCR_REF (R);
  return R;
}

#define BDD_DEPTH(f)	BDD_AUX1_L (f)

static void depth_action (BDDPTR v)
{
  if (BDD_TERM_P (v))
    BDD_DEPTH (v) = 0;
  else {
    int T_depth = BDD_DEPTH (BDD_THEN (v));
    int E_depth = BDD_DEPTH (BDD_ELSE (v));

    BDD_DEPTH (v) = 1 + (T_depth < E_depth ? E_depth : T_depth);
  }
}

int bdd_depth (BDDPTR f)
{
  if (BDD_VOID_P (f))
    return 0;

  bdd_traverse_post (f, depth_action);
  bdd_reset_marks (f);
  return BDD_DEPTH (f);
}

#undef BDD_DEPTH


/* New bdd_constrain (used to be in bdd_vfns.c).
   (Mis)uses computed table cache which is much bigger than original
   constrain cache.
   Also, allows dynamic variable ordering during recursive calls.
   But is this correct? No!
   27/9/95 Changed code again. Now not using any bdd function that might
   cause DVO ==> no longer need to explicitly switch it off.
*/

#define bdd_lookup_constrain_cache(a, b) \
	bdd_lookup_apply_cache (bdd_constrain, a, b)

#define bdd_insert_constrain_cache(a, b, R) \
	bdd_insert_apply_cache (bdd_constrain, a, b, R)

/* Image Restricting Generalized bdd_cofactor.

   constrain (f, v ) = constrain (fv , 1) = fv
   constrain (f, v') = constrain (fv', 1) = fv'
   constrain (f, ab) = constrain (fa, b) = constrain (fab, 1) = fab

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
  BDDPTR R;

  /* f | 1 = f, 1 | c = 1, 0 | c = 0, X | c = X */
  if (BDD_1_P (c) || BDD_TERM_P (f)) /* (BDD_X inclusive) */
    /* c is the universe, so no domain restriction in effect. */
    /* or f is domain independent */
    return bdd_assign (f);

  if (BDD_TERM_P (c))
    /* Type error. */
    return BDD_VOID;

#if 0
  /* It seems that these `speed-ups' in fact really slow the whole
     process down significantly! At least that's what Koen found out.
  */

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
#endif

  /* Memory function speed up: check cache: */
  if (!BDD_VOID_P (R = bdd_lookup_constrain_cache (f, c)))
    return bdd_assign (R);

  {
    int rank_f = BDD_RANK (f);
    int rank_c = BDD_RANK (c);

    /* c = v.T + v'.E */

    if (rank_f == rank_c) {
      BDDPTR T = BDD_COFACTOR_POS (c);
      BDDPTR E = BDD_COFACTOR_NEG (c);

      if (BDD_0_P (E) || BDD_X_P (E))
	/* c = 0 for var = 0: take f|var and constrain to c|var (= T): */
	R = bdd_constrain_aux (BDD_COFACTOR_POS (f), T);
      else
      if (BDD_0_P (T) || BDD_X_P (T))
	/* c = 0 for var = 1: take f|var' and constrain to c|var' (= E): */
	R = bdd_constrain_aux (BDD_COFACTOR_NEG (f), E);
      else {
	T = bdd_constrain_aux (BDD_COFACTOR_POS (f), T);
	E = bdd_constrain_aux (BDD_COFACTOR_NEG (f), E);
	R = bdd_create_node   (BDD_VARID (f), T, E);
	bdd_free (T);
	bdd_free (E);
      }
    }
    else
    if (rank_f < rank_c) {
      BDDPTR T, E;

      T = bdd_constrain_aux (BDD_COFACTOR_POS (f), c);
      E = bdd_constrain_aux (BDD_COFACTOR_NEG (f), c);
      R = bdd_create_node   (BDD_VARID (f), T, E);
      bdd_free (T);
      bdd_free (E);
    }
    else { /* rank_f > rank_c */
      BDDPTR T = BDD_COFACTOR_POS (c);
      BDDPTR E = BDD_COFACTOR_NEG (c);

      if (BDD_0_P (E) || BDD_X_P (E))
	/* c = 0 for var = 0: take f and constrain to c|var (= T): */
	R = bdd_constrain_aux (f, T);
      else
      if (BDD_0_P (T) || BDD_X_P (T))
	/* c = 0 for var = 1: take f and constrain to c|var' (= E): */
	R = bdd_constrain_aux (f, E);
      else {
	T = bdd_constrain_aux (f, T);
	E = bdd_constrain_aux (f, E);
	R = bdd_create_node   (BDD_VARID (c), T, E);
	bdd_free (T);
	bdd_free (E);
      }
    }
    return bdd_insert_constrain_cache (f, c, R);
  }
}

BDDPTR bdd_constrain (BDDPTR f, BDDPTR c)
{
  if (BDD_VOID_P (f) || BDD_VOID_P (c) || BDD_0_P (c) || BDD_X_P (c))
    /* Restriction to an empty set yields no function; it doesn't make
       sense to define a function on an empty domain. */
    return BDD_VOID;

  return bdd_constrain_aux (f, c);
}

/*
Not necessary. But why not? Because constrain cache is in fact computed table,
and computed table gets cleared after DVO anyway.
void bdd_flush_constrain_cache (void)
{
  register int i;
  register int size = BDD_CT_SIZE(computed_table);
  register COMPUTED_TABLE_ENTRY *entry = BDD_CT_ENTRIES(computed_table);

  for (i = 0; i < size; i++, entry++)
    if (!BDD_VOID_P (entry->R) && entry->F == BDD_F_SET (bdd_constrain_aux)) {
      entry->R = BDD_VOID;
      BDD_CT_ITEMS(computed_table)--;
    }
}
*/

#define bdd_lookup_restrict_cache(a, b) \
	bdd_lookup_apply_cache (bdd_restrict, a, b)

#define bdd_insert_restrict_cache(a, b, R) \
	bdd_insert_apply_cache (bdd_restrict, a, b, R)

static BDDPTR bdd_restrict_aux (BDDPTR f, BDDPTR c)
{
  BDDPTR R;

  if (BDD_1_P (c) || BDD_TERM_P (f))
    /* Restriction to the universe means no restriction at all. */
    /* Restricting a constant function has no effect. */
    return bdd_assign (f);

  if (BDD_TERM_P (c))
    /* Type error. */
    return BDD_VOID;

#if 0
  /* It seems that these `speed-ups' in fact really slow the whole
     process down significantly! At least that's what Koen found out.
  */

  /* Speed up 1: */
  if (BDD_COVERS (f, c))
    return bdd_1 ();

  /* Speed up 2: */
  {
    BDDPTR fnot = bdd_not (f);

    if (BDD_COVERS (fnot, c)) {
      bdd_free (fnot);
      return bdd_0 ();
    }
    bdd_free (fnot);
  }
#endif

  /* Memory function speed up: check cache: */
  if (!BDD_VOID_P (R = bdd_lookup_restrict_cache (f, c)))
    return bdd_assign (R);

  {
    int rank_f = BDD_RANK (f);
    int rank_c = BDD_RANK (c);

    if (rank_f == rank_c) {
      BDDPTR T = BDD_COFACTOR_POS (c);
      BDDPTR E = BDD_COFACTOR_NEG (c);

      if (BDD_0_P (E) || BDD_X_P (E))
	/* c = 0 for var = 0: take f|var and restrict to c|var: */
	R = bdd_restrict_aux (BDD_COFACTOR_POS (f), T);
      else
      if (BDD_0_P (T) || BDD_X_P (T))
	/* c = 0 for var = 1: take f|var' and restrict to c|var': */
	R = bdd_restrict_aux (BDD_COFACTOR_NEG (f), E);
      else {
	T = bdd_restrict_aux (BDD_COFACTOR_POS (f), T);
	E = bdd_restrict_aux (BDD_COFACTOR_NEG (f), E);
	R = bdd_create_node  (BDD_VARID (f), T, E);
	bdd_free (T);
	bdd_free (E);
      }
    }
    else
    if (rank_f < rank_c) {
      BDDPTR T, E;

      T = bdd_restrict_aux (BDD_COFACTOR_POS (f), c);
      E = bdd_restrict_aux (BDD_COFACTOR_NEG (f), c);
      R = bdd_create_node  (BDD_VARID (f), T, E);
      bdd_free (T);
      bdd_free (E);
    }
    else { /* rank_f > rank_c */
      BDDPTR T = BDD_COFACTOR_POS (c);
      BDDPTR E = BDD_COFACTOR_NEG (c);

      /* Avoid DVO: use bdd_ite_aux instead of bdd_or!
	 Don't know yet why can't use DVO, but know for sure that
	 with it errors occur and even C stack overflows.
      */
      R = bdd_restrict_aux (f, T = bdd_ite_aux (T, BDD_1, E));
      bdd_free (T);
    }
    return bdd_insert_restrict_cache (f, c, R);
  }
}

BDDPTR bdd_restrict (BDDPTR f, BDDPTR c)
{
  if (BDD_VOID_P (f) || BDD_VOID_P (c) || BDD_0_P (c) || BDD_X_P (c))
    /* Restriction to an empty set yields no function; it doesn't make
       sense to define a function on an empty domain. */
    return BDD_VOID;

  return bdd_restrict_aux (f, c);
}

/* ------------------------------------------------------------------------ */
/* DUMPING BDDS                                                             */
/* ------------------------------------------------------------------------ */

#ifndef BDD_LIGHT

/*
Dumping a vector of BDD's to an array or binary file.

Purpose: quickly obtain a compact description of the Boolean function
that the BDD represents and conversely be able to efficiently restore
the BDD from this description.

Method/Consideration: to avoid problems with variable ordering, allocation
etc. it must be possible to read in a BDD file when other BDD's already
exist in memory. Still if the ordering is not changed the construction
of the read-in BDD should be linear in its size.
Use bdd_ite to reconstruct the BDD, so best is to read (and thus also write)
the description in a depth-first post-order way (in fact a reversed
topological ordering).
The easiest is to keep the description symbolic.
The nodes BDD_0, BDD_1, and BDD_X can be treated implicitly. We have to
reserve some special symbols to distinguish them from ordinary, i.e. internal
BDD node references. Also the BDD_VOID value must be dumped in a special way.

We can save space upon restore when node references are reused. This is
possible when we determine upon dumping when a node is no longer referenced;
then obviously its symbolic reference as written to the file might be reused
for other subsequent nodes. The actual number of these references used needs
of course also to be output because the loader requires this information to
reserve that much space. And also we need a mechanism for the loader to know
the association between reconstructed BDD and its symbolic file reference.
However, if this cannot be established in a simple way it would require extra
information per node in the file, namely the index where the BDD for this node
is to be stored.
One approach would be: order nodes in breadth-first levels, i.e. the nodes
per level have the same shortest path to the root. Within a level the nodes
are topologically sorted. There will be no edges crossing a level.
It is enough to allocate a number of references that is twice the number of
nodes in the fullest level. Use these references to number the nodes
modulo the number of references starting at the lowest level and within
a level taking the toplogical order into account.
THE APPROACH PROPOSED ABOVE DOESN'T WORK!

The dump array/file consists of:
1. Number of internal nodes dumped, 4 bytes
2. Number of BDDs dumped (in a case of a vector dump), 4 bytes
3. Number of variables (bdd_nr_vars) in use at time of dumping, 4 bytes
4. The internal node dump records, see below, 8 bytes per node
5. Edge info for each vector element, 3 bytes per edge

Things that are recorded for each internal BDD node:
1. A reference to its variable id,
2. A reference to the then node together with possible complement bit,
3. A reference to the else node together with possible complement and
   inverted input bits.

Currently we use
1. 2 bytes, 16 bits for variable id,
2. 3 bytes, 2 bits reserved, 22 left over for reference
3. 3 bytes, 2 bits reserved, 22 left over for reference
In total 8 bytes per node.
This means that worst-case a graph of at most 2^22 * 8 = 32Mbytes file size
can be dumped. Internally such a graph would require at least 3 times as much
space.
*/

/* Status of the DUMP/LOAD code is experimental! */

/* Pack an internal BDD node's data in a 8-byte char array b.
   Only allowing upto 2^16 variable id's. No check!
   Only allowing upto 2^22 different BDD nodes. No check!

 b\bits  7   6   5   4   3   2   1   0
       +-------------------------------+
     0 |         VARID    MSBs  15-8   | 
       +-------------------------------+
     1 |         VARID    LSBs   7-0   | 
       +---+---+-----------------------+
     2 | I | X | THEN     MSBs  22-16  |
       +---+---+-----------------------+
     3 |         THEN           15-8   | 
       +-------------------------------+
     4 |         THEN     LSBs   7-0   | 
       +---+---+-----------------------+
     5 | I | O | ELSE     MSBs  21-16  | 
       +---+---+-----------------------+
     6 |         ELSE           15-8   | 
       +-------------------------------+
     7 |         ELSE     LSBs   7-0   | 
       +-------------------------------+
*/
static int node_id;

static void bdd_dump_int (unsigned char *b, int n)
{
  /* Can't use *(int *)b = n for alignment reasons! */
  b[0] = n >> 24;
  b[1] = n >> 16;
  b[2] = n >>  8;
  b[3] = n;
}

static int bdd_restore_int (unsigned char *b)
{
  return (b[0] << 24) | (b[1] << 16) | (b[2] << 8) | b[3];
}

static void bdd_dump_node (unsigned char *b, BDDPTR v)
{
  b[0] = BDD_VARID (v) >>  8;
  b[1] = BDD_VARID (v);
  b[2] = BDD_AUX1_L (BDD_THEN (v)) >> 16;
  b[3] = BDD_AUX1_L (BDD_THEN (v)) >>  8;
  b[4] = BDD_AUX1_L (BDD_THEN (v));
  b[2] |= (BDD_I_INV_EDGE_P (BDD_THEN (v)) << 7);
  b[5] = BDD_AUX1_L (BDD_ELSE (v)) >> 16;
  b[6] = BDD_AUX1_L (BDD_ELSE (v)) >>  8;
  b[7] = BDD_AUX1_L (BDD_ELSE (v));
  b[5] |= (BDD_I_INV_EDGE_P (BDD_ELSE (v)) << 7);
  b[5] |= (BDD_O_INV_EDGE_P (BDD_ELSE (v)) << 6);

  /* Temp save symbolic address assigned to this node: */
  BDD_AUX1_L (v) = node_id++;
}

static void bdd_dump_edge (unsigned char *b, BDDPTR f)
{
  if (BDD_VOID_P (f)) {
    b[0] = 0;
    b[1] = 0;
    b[2] = 0;
    b[0] |= 1 << 7;
    b[0] |= 1 << 6;
  }
  else {
    b[0] = BDD_AUX1_L (f) >> 16;
    b[1] = BDD_AUX1_L (f) >>  8;
    b[2] = BDD_AUX1_L (f);
    b[0] |= (BDD_I_INV_EDGE_P (f) << 7);
    b[0] |= (BDD_O_INV_EDGE_P (f) << 6);
  }
}

/* Restores the BDD node that is packed in the 8-byte array b.
   Uses the nodes array to find the BDDs of its then and else edges.
*/
static BDDPTR bdd_restore_node (unsigned char *b, BDDPTR *nodes)
{
  int var;
  int e_I, e_O, t_I;
  int then_idx;
  int else_idx;
  BDDPTR v, T, E, R;

  var = (b[0] << 8) | b[1];
  t_I = b[2] & 0x80;
  then_idx = ((b[2] & 0x3F) << 16) | (b[3] << 8) | b[4];
  e_I = b[5] & 0x80;
  e_O = b[5] & 0x40;
  else_idx = ((b[5] & 0x3F) << 16) | (b[6] << 8) | b[7];

  v = bdd_create_var (var);
  T = bdd_assign (nodes[then_idx]);
  E = bdd_assign (nodes[else_idx]);
  if (t_I) {
    BDDPTR s;

    T = bdd_invert_input_top (s = T);
    bdd_free (s);
  }
  if (e_I) {
    BDDPTR s;

    E = bdd_invert_input_top (s = E);
    bdd_free (s);
  }
  if (e_O) {
    BDDPTR s;

    E = bdd_not (s = E);
    bdd_free (s);
  }
  R = bdd_ite (v, T, E);
  bdd_free (v);
  bdd_free (T);
  bdd_free (E);
  return R;
}

static BDDPTR bdd_restore_edge (unsigned char *b, BDDPTR *nodes)
{
  int inv;
  int not;
  int node_idx;
  BDDPTR f;

  inv = b[0] & 0x80;
  not = b[0] & 0x40;
  node_idx = ((b[0] & 0x3F) << 16) | (b[1] << 8) | b[2];

  if (inv && not && !node_idx)
    return BDD_VOID;

  f = bdd_assign (nodes[node_idx]);
  if (inv) {
    BDDPTR s;

    f = bdd_invert_input_top (s = f);
    bdd_free (s);
  }
  if (not) {
    BDDPTR s;

    f = bdd_not (s = f);
    bdd_free (s);
  }
  return f;
}

static unsigned char *store;

static void bdd_dump_to_chars_action (BDDPTR v)
{
  if (BDD_INTERN_P (v)) {
    bdd_dump_node (store, v);
    store += 8;
  }
}

unsigned char *bdd_dump_to_chars_vec (BDDPTR *f_vec, int size)
{
  unsigned char *result;
  int i, n;

  /* Constants have fixed symbolic addresses: */
  BDD_AUX1_L (BDD_0) = 0;
  /* Note: perhaps BDD_0 and BDD_1 are 1 and the same node; then
     edge to it will differentiate them.
  */
  BDD_AUX1_L (BDD_1) = 1;
  BDD_AUX1_L (BDD_X) = 2;
  /* First symbolic address for internal node: */
  node_id = 3;

  /* Calculate total number of internal nodes to be dumped: */
  node_counter = 0;
  global_ceiling = INT_MAX;

  /* Assumes all mark fields are 0. */
  for (i = 0; i < size; i++) {
    BDDPTR f = f_vec[i];

    if (!BDD_VOID_P (f) && !BDD_MARK (f))
      bdd_traverse_pre (f, count_nodes_no_consts);
  }
  /* Now all mark fields are set. */

  n = node_counter * 8 + 3 * sizeof (int) + size * 3;
  check_mem_limit (n);
  store = result = BDD_MALLOC_ARRAY (n, unsigned char);

  /* Dump total number of internal nodes of f_vec: */
  bdd_dump_int (store, node_counter);
  store += sizeof (int);

  /* Dump size of BDD vector f_vec: */
  bdd_dump_int (store, size);
  store += sizeof (int);

  /* Dump number of variables currently in use: */
  bdd_dump_int (store, bdd_nr_vars);
  store += sizeof (int);

  /* Dump all the internal nodes: */
  for (i = 0; i < size; i++) {
    BDDPTR f = f_vec[i];

    /* Note: vector of BDD's could share vertices. */
    if (!BDD_VOID_P (f) && BDD_MARK (f))
      bdd_traverse_post (f, bdd_dump_to_chars_action);
  }
  /* Now all mark fields are 0 again. */

  /* Dump edges to nodes of interest: */
  for (i = 0; i < size; i++) {
    bdd_dump_edge (store, f_vec[i]);
    store += 3;
  }

  /* Clean up the AUX_L fields: */
  /* Assumes all mark fields are 0. */
  for (i = 0; i < size; i++) {
    BDDPTR f = f_vec[i];

    if (!BDD_VOID_P (f) && !BDD_MARK (f))
      bdd_traverse_pre (f, bdd_reinit_aux1_action);
  }
  /* Now all mark fields are set. */

  /* Assumes all mark fields are 1. */
  for (i = 0; i < size; i++) {
    BDDPTR f = f_vec[i];

    if (!BDD_VOID_P (f) && BDD_MARK (f))
      bdd_traverse_pre (f, bdd_null_action);
  }
  /* Now all mark fields are 0 again. */

  return result;
}

BDDPTR *bdd_restore_from_chars_vec (unsigned char *b, BDDPTR *f_vec, int *len)
{
  unsigned char *store = b;
  int i;
  int size;
  int nr_vars;
  int nr_nodes;
  BDDPTR *nodes;

  nr_nodes = bdd_restore_int (store);
  store += sizeof (int);

  size = bdd_restore_int (store);
  store += sizeof (int);

  nr_vars = bdd_restore_int (store);
  store += sizeof (int);

  check_mem_limit ((nr_nodes+3) * sizeof (BDDPTR));

  nodes = BDD_MALLOC_ARRAY (nr_nodes+3, BDDPTR);
  nodes[0] = bdd_assign (BDD_0);
  nodes[1] = bdd_assign (BDD_1);
  nodes[2] = bdd_assign (BDD_X);

  for (i = 3; i < nr_nodes+3; i++) {
    nodes[i] = bdd_restore_node (store, nodes);
    store += 8;
  }

  if (!f_vec) {
    check_mem_limit (size * sizeof (BDDPTR));
    f_vec = BDD_MALLOC_ARRAY (size, BDDPTR);
  }

  for (i = 0; i < size; i++) {
    f_vec[i] = bdd_restore_edge (store, nodes);
    store += 3;
  }

  for (i = 0; i < nr_nodes+3; i++)
    bdd_free (nodes[i]);
  BDD_FREE_ARRAY (nodes, nr_nodes+3, BDDPTR);

  if (len) *len = size;
  return f_vec;
}

void bdd_free_dumped_chars (unsigned char *b)
{
  unsigned char *store = b;
  int n;
  int size;
  int nr_nodes;

  nr_nodes = bdd_restore_int (store);
  store += sizeof (int);

  size = bdd_restore_int (store);

  n = nr_nodes * 8 + 3 * sizeof (int) + size * 3;

  BDD_FREE_ARRAY (b, n, unsigned char);
}

#endif /*BDD_LIGHT*/

/* ------------------------------------------------------------------------ */
/* DUMPING BDDS TO A FILE                                                   */
/* ------------------------------------------------------------------------ */

#if 0

static void bdd_dump_to_file_action (BDDPTR v)
{
  if (BDD_INTERN_P (v)) {
    unsigned char b[8];

    bdd_dump_node (b, v);
    fwrite ((const void *)b, (size_t) 1, (size_t) 8, global_fp);
  }
}

/*
void bdd_dump_to_file_vec (FILE *fp, BDDPTR *f_vec, int size)

Writes a vector of BDDs `f_vec' of length `size' to the open file described
by the file pointer `fp'.
The data is written in binary and is read again by the routine
bdd_restore_from_file_vec.

*/
void bdd_dump_to_file_vec (FILE *fp, BDDPTR *f_vec, int size)
{
  int i;

  /* Constants have fixed symbolic addresses: */
  BDD_AUX1_L (BDD_0) = 0;
  /* Note: perhaps BDD_0 and BDD_1 are 1 and the same node; then
     edge to it will differentiate them.
  */
  BDD_AUX1_L (BDD_1) = 1;
  BDD_AUX1_L (BDD_X) = 2;
  /* First symbolic address for internal node: */
  node_id = 3;

  /* Calculate total number of internal nodes to be dumped: */
  node_counter = 0;
  global_ceiling = INT_MAX;

  /* Assumes all mark fields are 0. */
  for (i = 0; i < size; i++) {
    BDDPTR f = f_vec[i];

    if (!BDD_VOID_P (f) && !BDD_MARK (f))
      bdd_traverse_pre (f, count_nodes_no_consts);
  }
  /* Now all mark fields are set. */

  /* Dump total number of internal nodes of f_vec: */
  fwrite ((const void *)&node_counter, (size_t) sizeof (int), (size_t) 1, fp);

  /* Dump size of BDD vector f_vec: */
  fwrite ((const void *)&size, (size_t) sizeof (int), (size_t) 1, fp);

  /* Dump number of variables currently in use: */
  fwrite ((const void *)&bdd_nr_vars, (size_t) sizeof (int), (size_t) 1, fp);

  /* Dump all the internal nodes: */
  global_fp = fp;
  for (i = 0; i < size; i++) {
    BDDPTR f = f_vec[i];

    /* Note: vector of BDD's could share vertices. */
    if (!BDD_VOID_P (f) && BDD_MARK (f))
      bdd_traverse_post (f, bdd_dump_to_file_action);
  }
  /* Now all mark fields are 0 again. */

  /* Dump edges to nodes of interest: */
  for (i = 0; i < size; i++) {
    unsigned char b[3];

    bdd_dump_edge (b, f_vec[i]);
    fwrite ((const void *)b, (size_t) 1, (size_t) 3, fp);
  }

  /* Clean up the AUX_L fields: */
  /* Assumes all mark fields are 0. */
  for (i = 0; i < size; i++) {
    BDDPTR f = f_vec[i];

    if (!BDD_VOID_P (f) && !BDD_MARK (f))
      bdd_traverse_pre (f, bdd_reinit_aux1_action);
  }
  /* Now all mark fields are set. */

  /* Assumes all mark fields are 1. */
  for (i = 0; i < size; i++) {
    BDDPTR f = f_vec[i];

    if (!BDD_VOID_P (f) && BDD_MARK (f))
      bdd_traverse_pre (f, bdd_null_action);
  }
  /* Now all mark fields are 0 again. */
}

/*
BDDPTR *bdd_restore_from_file_vec (FILE *fp, BDDPTR *f_vec, int *len)

Reads a vector of BDDs as written by the routine bdd_dump_to_file_vec from the
open file `fp'.
If `f_vec' is non-NULL it is assumed to point to an area large enough
to store the number of BDDs contained in the file; if the argument is NULL,
this routine allocates the required space and returns a pointer to it.
If `len' is non-0, the number of read BDDs is assigned to it.

*/
BDDPTR *bdd_restore_from_file_vec (FILE *fp, BDDPTR *f_vec, int *len)
{
  int i;
  int size;
  int nr_vars;
  int nr_nodes;
  BDDPTR *nodes;
  unsigned char b[9];

  fread (&nr_nodes, (size_t) sizeof (int), (size_t) 1, fp);

  fread (&size, (size_t) sizeof (int), (size_t) 1, fp);

  fread (&nr_vars, (size_t) sizeof (int), (size_t) 1, fp);

  check_mem_limit ((nr_nodes+3) * sizeof (BDDPTR));

  nodes = BDD_MALLOC_ARRAY (nr_nodes+3, BDDPTR);
  nodes[0] = bdd_assign (BDD_0);
  nodes[1] = bdd_assign (BDD_1);
  nodes[2] = bdd_assign (BDD_X);

  for (i = 3; i < nr_nodes+3; i++) {
    fread (b, (size_t) 1, (size_t) 8, fp);
    nodes[i] = bdd_restore_node (b, nodes);
  }

  if (!f_vec) {
    check_mem_limit (size * sizeof (BDDPTR));
    f_vec = BDD_MALLOC_ARRAY (size, BDDPTR);
  }

  for (i = 0; i < size; i++) {
    fread (b, (size_t) 1, (size_t) 3, fp);
    f_vec[i] = bdd_restore_edge (b, nodes);
  }

  for (i = 0; i < nr_nodes+3; i++)
    bdd_free (nodes[i]);
  BDD_FREE_ARRAY (nodes, nr_nodes+3, BDDPTR);

  if (len) *len = size;
  return f_vec;
}

#endif

/* ------------------------------------------------------------------------ */
/* DYNAMIC VARIABLE ORDERING                                                */
/* ------------------------------------------------------------------------ */

/* Pre: 0 <= v < BDD_TERMID, !BDD_VOID_P (T) && !BDD_VOID_P (E).
        !bdd_use_inv_edges
	T != E ==> rank (v) < rank(top(T)) && rank (v) < rank(top(E))

   Note: always increments T and E nodes' ref count by 1.
   Return value might have bit0 set!
*/
static BDDPTR bdd_create_node_1 (int v, BDDPTR T, BDDPTR E)
{
  int neg_result = 0;

  if (BDD_EQUAL_P (T, E)) {
    BDD_INCR_REF (T);
    return T;
  }

  /* Here: !BDD_VOID_P (T) && !BDD_VOID_P (E),
	   T != E,
	   0 <= v < BDD_TERMID.
  */

  if (BDD_TERM_P (T) && !BDD_BOOL_P (T)) {
    if (BDD_O_INV_EDGE_P (E)) {
      neg_result = 1;
      E = BDD_O_OFF (E);
    }
  }
  else
  if (BDD_O_INV_EDGE_P (T)) {
    neg_result = 1;
    T = BDD_O_OFF (T);
    E = BDD_COMPL (E);
  }

  /* Here: !BDD_VOID_P (T) && !BDD_VOID_P (E),
           T positive,
	   T != E,
	   0 <= v < BDD_TERMID.
  */
  {
    V_HASHTAB tab = unique_table.space[BDD_VAR_RANK (v)];
    int index = hash_U (T, E, BDD_VUT_LOG2SIZE(tab));
    register BDDPTR new;

    /* Lookup in bucket: */
    for (new = BDD_VUT_ENTRIES(tab)[index]; new; new = BDD_NEXT (new))
      if (MATCHES (new, v, T, E)) {
/*	assert (!BDD_DEAD_P (new));*/
/*
	if (BDD_DEAD_P (new)) {
	  BDD_INCR_REF (T);
	  BDD_INCR_REF (E);
	}
*/
	/* new is non-constant. */
	/* new has modification bits off. */
	BDD_INCR_REF (new);
	return neg_result ? BDD_O_SET_U (new) : new;
      } /*if-for*/

    /* New entry: */
    if (unique_table.nr_items >= bdd_nr_nodes_allowed)
      return bdd_nodes_limit_handler ();

    new            = BDD_MALLOC ();
    /* new of course has modification bits off. */
    new->varid     = v;
    new->mark      = 0;
    new->flag      = 0;
    /* Protect new from gc: */
    new->refcount  = 1;
    /* Let's be nice and ensure that aux fields are initialized: */
    BDD_AUX1 (new) = NULL_AUX_FIELD;
    BDD_AUX2 (new) = NULL_AUX_FIELD;
    new->then_link = T;
    BDD_INCR_REF (T);
    new->else_link = E;
    BDD_INCR_REF (E);

    /* Link to unique table: */
    BDD_NEXT (new) = BDD_VUT_ENTRIES(tab)[index];
    BDD_VUT_ENTRIES(tab)[index] = new;
    BDD_VUT_ITEMS(tab)++;
    if (++unique_table.nr_items > bdd_peak_nr_nodes)
      bdd_peak_nr_nodes++;

    if (unique_table.nr_items - bdd_nr_dead_nodes > bdd_peak_nr_nodes_alive)
      bdd_peak_nr_nodes_alive = unique_table.nr_items - bdd_nr_dead_nodes;

    return neg_result ? BDD_O_SET_U (new) : new;
  }
}

/* Pre: F in unique_table and DEAD. */
static void bdd_local_gc (BDDPTR F)
{
  V_HASHTAB tab = unique_table.space[BDD_RANK (F)];
  BDDPTR      T = BDD_THEN (F);
  BDDPTR      E = BDD_ELSE (F);
  int     index = hash_U (T, E, BDD_VUT_LOG2SIZE(tab));
  BDDPTR *chain = BDD_VUT_ENTRIES(tab) + index;
  BDDPTR   node = *chain;

  while (node) {
    if (BDD_EQUAL_P (PTR (F), node)) {
      BDD_DECR_REF (T);
      BDD_DECR_REF (E);

      /* Bypass node to be removed: */
      *chain = BDD_NEXT (node);
      BDD_FREE (node);

      /* This table now has 1 item less: */
      BDD_VUT_ITEMS(tab)--;
      unique_table.nr_items--;
      return;
    } /*if*/
    chain = &(BDD_NEXT (node));
    node = *chain;
  } /*while*/

  /* Seems not to work on RS 6000 AIX:
  assert (!"Fatal: Dead node F not found during local GC.");
  */

  fputs ("[bdd_local_gc]: Fatal error: Dead node F not found.\n", stderr);
  assert (0);
}

/* Visits all the nodes on a particular level (rank) and
   performs a swap with the variables on the next level.
   Only manipulates the BDD nodes and the chains they belong to, does not
   effectuate the swap in the unique and rank table data structures.
*/
static void bdd_process_level (int rank)
{
  int      next_rank = rank + 1;
  register int index;
  V_HASHTAB      tab = unique_table.space[rank];
  BDDPTR      *entry = BDD_VUT_ENTRIES(tab);
  int              v = BDD_VUT_ID(tab);
  register int  size = pow2(BDD_VUT_LOG2SIZE(tab));
  V_HASHTAB  new_tab = unique_table.space[next_rank];
  int   new_log2size = BDD_VUT_LOG2SIZE(new_tab);
  int      new_index;

  /* Process all nodes in table for rank: */
  for (index = 0; index < size; index++, entry++) {
    BDDPTR *chain = entry;
    BDDPTR      F = *chain;

    /* Process the chain of nodes at this entry: */
    while (F) {
      BDDPTR     F1 = BDD_THEN (F); /* perhaps constant */
      BDDPTR     F0 = BDD_ELSE (F); /* perhaps constant */
      int then_rank = BDD_RANK (F1);
      int else_rank = BDD_RANK (F0);

      if (then_rank != next_rank && else_rank != next_rank) {
	/* Skip this node and get the next one: */
	chain = &(BDD_NEXT (F));
	F = *chain;
	continue;
      }

      /* Unlink F from current chain in tab: */
      *chain = BDD_NEXT (F);

      /* This table now has 1 item less: */
      BDD_VUT_ITEMS(tab)--;
      unique_table.nr_items--;

      if (then_rank == next_rank && else_rank == next_rank) {
	/* F1, F0 non-constant, perhaps point to same node. */
	BDDPTR F11 = BDD_THEN (F1);
	BDDPTR F10 = BDD_ELSE (F1);
	BDDPTR F01 = BDD_THEN (F0);
	BDDPTR F00 = BDD_ELSE (F0);

	if (BDD_O_INV_EDGE_P (F0)) {
	  F01 = BDD_O_SET (F01);
	  F00 = BDD_COMPL (F00);
	}

	BDD_DECR_REF (F1);
	BDD_DECR_REF (F0);

	if (BDD_DEAD_P (F0))
	  bdd_local_gc (F0);

	/* Avoid removing same node twice. */
	if (F1 != PTR (F0) && BDD_DEAD_P (F1))
	  bdd_local_gc (F1);

	F1 = BDD_THEN (F) = bdd_create_node_1 (v, F11, F01);
	F0 = BDD_ELSE (F) = bdd_create_node_1 (v, F10, F00);
      }
      else
      if (else_rank == next_rank) {
	BDDPTR F01 = BDD_THEN (F0);
	BDDPTR F00 = BDD_ELSE (F0);

	if (BDD_O_INV_EDGE_P (F0)) {
	  F01 = BDD_O_SET (F01);
	  F00 = BDD_COMPL (F00);
	}

	BDD_DECR_REF (F0);
	if (BDD_DEAD_P (F0))
	  bdd_local_gc (F0);

	BDD_DECR_REF (F1);
	/* Watch out! F0 depends on original F1. */
	F0 = BDD_ELSE (F) = bdd_create_node_1 (v, F1, F00);
	F1 = BDD_THEN (F) = bdd_create_node_1 (v, F1, F01);
      }
      else { /* then_rank == next_rank */
	BDDPTR F11 = BDD_THEN (F1);
	BDDPTR F10 = BDD_ELSE (F1);

	BDD_DECR_REF (F1);
	if (BDD_DEAD_P (F1))
	  bdd_local_gc (F1);

	BDD_DECR_REF (F0);
	F1 = BDD_THEN (F) = bdd_create_node_1 (v, F11, F0);
	F0 = BDD_ELSE (F) = bdd_create_node_1 (v, F10, F0);
      }
/*
      assert (F1 != F0);
      assert (BDD_POS_P (F1));
      assert (BDD_X_P (F1) <= BDD_POS_P (F0));
*/
      /* Move F to table of next_rank: */
      F->varid = BDD_VUT_ID(new_tab);

      new_index = hash_U (F1, F0, new_log2size);

      /* Link to the head of the entry list of the new Hash table: */
      BDD_NEXT (F) = BDD_VUT_ENTRIES(new_tab)[new_index];
      BDD_VUT_ENTRIES(new_tab)[new_index] = F;
      BDD_VUT_ITEMS(new_tab)++;
      unique_table.nr_items++;

      /* Determine load factor: */
      if (((float) BDD_VUT_ITEMS(new_tab)) / pow2(new_log2size)
	  > BDD_LOAD_FACTOR) {
	new_tab      = bdd_resize_hash_table (new_tab, 1);
	new_log2size = BDD_VUT_LOG2SIZE(new_tab);
      }

      /* Consider next node in chain: */
      F = *chain;
    } /*while*/
  } /*for*/
}

static int comp (const int *p, const int *q)
{
  return BDD_VUT_ITEMS(unique_table.space[BDD_VAR_RANK (*q)])
       - BDD_VUT_ITEMS(unique_table.space[BDD_VAR_RANK (*p)]);
}

#ifdef COMMENT
static int count_frozen (void)
{
  register int i;
  register int count = 0;

  for (i = 0; i < unique_table.count; i++) {
    V_HASHTAB tab = unique_table.space[i];

    if (tab) {
      register int j;
      register int size = BDD_VUT_SIZE(tab);
      register BDDPTR *entry = BDD_VUT_ENTRIES(tab);

      for (j = 0; j < size; j++, entry++) {
	register BDDPTR node  = *entry;

	while (node) {
	  if (BDD_FROZEN_P (node))
	    count++;
	  node = BDD_NEXT (node);
	} /*while*/
      } /*for*/
    } /*if*/
  } /*for i*/

  {
    BDDPTR *vec = BDD_MALLOC_ARRAY (count, BDDPTR);

    count = 0;

    for (i = 0; i < unique_table.count; i++) {
      V_HASHTAB tab = unique_table.space[i];

      if (tab) {
	register int j;
	register int size = BDD_VUT_SIZE(tab);
	register BDDPTR *entry = BDD_VUT_ENTRIES(tab);

	for (j = 0; j < size; j++, entry++) {
	  register BDDPTR node  = *entry;

	  while (node) {
	    if (BDD_FROZEN_P (node))
	      vec[count++] = node;
	    node = BDD_NEXT (node);
	  } /*while*/
	} /*for*/
      } /*if*/
    } /*for i*/

    return bdd_size_vec (vec, count);
  }
}

static int check_flag;
static void check_action (BDDPTR v)
{
  if (BDD_INTERN_P (v) && BDD_THEN (v) == BDD_ELSE (v))
    check_flag = 0;
}

static int bdd_check_struct (BDDPTR f)
{
  check_flag = 1;
  bdd_traverse_pre (f, check_action);
  bdd_reset_marks (f);
  return check_flag;
}

static int check_unique_table (void)
{
  register int i;
  register V_HASHTAB *tab = unique_table.space;

  for (i = 0; i < unique_table.count; i++, tab++) {
    if (*tab) {
      register int j;
      register int size = BDD_VUT_SIZE(*tab);
      register BDDPTR *entry = BDD_VUT_ENTRIES(*tab);

      for (j = 0; j < size; j++, entry++) {
	register BDDPTR node = *entry;

	while (node) {
	  if (   !BDD_DEAD_P (node)
	      && !BDD_NEG_P (BDD_THEN (node))
	      && !(BDD_NEG_P (BDD_ELSE (node))
		   <= !BDD_X_P (PTR (BDD_ELSE (node))))
	      && !(BDD_X_P (BDD_THEN (node)) <= BDD_POS_P (BDD_ELSE (node)))
	      && BDD_THEN (node) == BDD_ELSE (node)) {
fprintf (stderr, "Fatal: table for v %d, rank %d.\n", BDD_VARID (node), i);
	    return 0;
          }
	  node = BDD_NEXT (node);
	} /*while*/
      } /*for*/
    } /*if*/
  } /*for i*/
  return 1;
}
#endif

/* Check whether any impossible swap situations arise.
   This happens when the THEN link of the THEN link of a BDD node is BDD_X and
   and the ELSE link is negative. In those cases it is impossible to perform
   a correct swap because the result can no longer be made canonical.
   (When a BDD_X appears as THEN node then the ELSE node must be not
   complemented!)
   For user-introduced terminal nodes we will have the same problem;
   this is not taken into account yet!
   Could avoid doing this when assured that BDD_X is not used!
   Doing a fast check now whether BDD_X is referenced. Assumes therefore
   that everybody is correctly handling this constant node, i.e., using
   bdd_assign to get a copy, and using bdd_free to free it again.
   One should thus not rely on the fact that constant nodes need not be
   garbage-protected since they won't be actually ever freed anyway.
*/
static int rank_swap_impossible (int rank)
{
  register int i;
  V_HASHTAB          tab = unique_table.space[rank];
  register int      size = BDD_VUT_SIZE(tab);
  register BDDPTR *entry = BDD_VUT_ENTRIES(tab);

  if (bdd_use_neg_edges && BDD_REFCOUNT (BDD_X) > 0)
    for (i = 0; i < size; i++, entry++) {
      register BDDPTR node = *entry;

      while (node) {
	if (   BDD_NEG_P    (BDD_ELSE (node))
	    && BDD_INTERN_P (BDD_THEN (node))
	    && BDD_X_P      (BDD_THEN (BDD_THEN (node))))
	  return 1;
	node = BDD_NEXT (node);
      } /*while*/
    } /*for-if*/
  return 0;
}

#define BDD_SWAP_RANKS(rank)	\
do {\
  bdd_process_level (rank);\
\
  {\
    int next_rank = (rank) + 1; \
    V_HASHTAB tab1 = unique_table.space[rank];\
    V_HASHTAB tab2 = unique_table.space[next_rank];\
\
    unique_table.ranks[BDD_VUT_ID(tab1)] = next_rank;\
    unique_table.ranks[BDD_VUT_ID(tab2)] = rank;\
\
    unique_table.space[rank]      = tab2;\
    unique_table.space[next_rank] = tab1;\
  }\
} once

/* Reorder the variables in a group.
   The group (or subset) is specified by its starting and ending rank values.
   No action when the group consist of none or a single variable.

   Returns number of BDD nodes gained, i.e., the number of alive nodes at
   the start minus the number of alive nodes we end up with.
   Could in some very unlikely cases result in a negative number. Then
   probably some nodes got frozen in the meantime.
*/
static int bdd_dynamic_order_group (int start_rank, int end_rank)
{
  int size = end_rank - start_rank + 1;
  int *varids;
  int i, k;
  int alive_before, alive_after;
  int total = 0;
  /* Threshold factor in increase of best size beyond which swapping is
     stopped. This bounds any hill-climbing.
     For now allow 5 % increase in size, i.e., number of BDD nodes.
  */
  static float THRESHOLD = 1.05;

  if (size <= 1)
    return 0;

  alive_before = unique_table.nr_items;

  /* Sort var ids according non-increasing number of occurrences of the
     associated BDD nodes BDD_VUT_ITEMS(unique_table.space[rank]).
  */
  check_mem_limit (size * sizeof (int));

  varids = BDD_MALLOC_ARRAY (size, int);

  for (i = start_rank, k = 0; k < size; i++, k++)
    varids[k] = BDD_VUT_ID(unique_table.space[i]);

  qsort ((void *) varids, (size_t) size, (size_t) sizeof (int),
	 (int (*) (const void *, const void *)) comp);

  /* Find last non-empty var: */
  while (k > 0
	 && !BDD_VUT_ITEMS(unique_table.space[BDD_VAR_RANK (varids[--k])]))
    ;

  if (bdd_verbose)
    fprintf (stderr,
	     "Dynamic variable ordering (%d vars, %d nodes)...",
	     k+1, alive_before);

  /* For each variable in order of non-increasing number of occurrences
     as BDD nodes:
  */
  for (i = 0; i <= k; i++) {
    int best_size = unique_table.nr_items;
    int         v = varids[i];
    int orig_rank = BDD_VAR_RANK (v);
    int      rank = orig_rank;
    int best_rank = orig_rank;

    /* Bound the total `time' spent in dynamic ordering.
       Simply stop after a certain number of variables has been considered.
    */
    total += best_size;
    /* 1<<25 =~  34*10^6 */
    /* 1<<28 =~ 268*10^6 */
    /* 1<<30 =~ 10^9 */
    if (total > ((float) (1 << 28)) / (k + 1))
      break;

    /* Because of swapping, some nodes might get frozen.
       These can never be freed again. Therefore it is possible that
       when a variable is put back at its best rank position
       the total number of nodes in the unique table is more than
       what we initially found.
       What is even worse is that a frozen node may have non-terminal
       children that also become unfreeable although they need not
       be frozen themselves.
    */
#if defined(BDD_DEBUG)
fprintf (stderr, "Orig: v = %3d, rank(v) = %3d, %12d nodes, %4d frozen.\n",
	 v, rank, unique_table.nr_items, bdd_nr_frozen_nodes);
#endif

    /* Down. */
    /* No need to swap the last but one variable with the last one now
       because this will happen anyway when the last variable is under
       consideration.
    */
    while (rank < end_rank-1) {
      if (rank_swap_impossible (rank))
	break;

      BDD_SWAP_RANKS (rank);
      rank++;

      if (unique_table.nr_items < best_size) {
	best_size = unique_table.nr_items;
	best_rank = rank;
      }
      else
      if (unique_table.nr_items > THRESHOLD * best_size)
	break;
    }

#if defined(BDD_DEBUG)
fprintf (stderr, "Down: v = %3d, rank(v) = %3d, %12d nodes, %4d frozen.\n",
	 v, rank, unique_table.nr_items, bdd_nr_frozen_nodes);
#endif

    /* Back up to original rank: */
    while (rank > orig_rank) {
      if (rank_swap_impossible (rank-1))
	break;

      rank--;
      BDD_SWAP_RANKS (rank);
    }
    /* Here: rank == orig_rank. */

#if defined(BDD_DEBUG)
fprintf (stderr, "Back: v = %3d, rank(v) = %3d, %12d nodes, %4d frozen.\n",
	 v, rank, unique_table.nr_items, bdd_nr_frozen_nodes);
#endif

    /* Up. */
    while (rank > start_rank) {
      if (rank_swap_impossible (rank-1))
	break;

      rank--;
      BDD_SWAP_RANKS (rank);

      if (unique_table.nr_items < best_size) {
	best_size = unique_table.nr_items;
	best_rank = rank;
      }
      else
      if (unique_table.nr_items > THRESHOLD * best_size)
	break;
    }

#if defined(BDD_DEBUG)
fprintf (stderr, "Up:   v = %3d, rank(v) = %3d, %12d nodes, %4d frozen.\n",
	 v, rank, unique_table.nr_items, bdd_nr_frozen_nodes);
#endif

    /* Back down to best rank: */
    while (rank < best_rank) {
      if (rank_swap_impossible (rank))
	break;

      BDD_SWAP_RANKS (rank);
      rank++;
    }
    /* Here: rank == best_rank. */

#if defined(BDD_DEBUG)
fprintf (stderr, "Best: v = %3d, rank(v) = %3d, %12d nodes, %4d frozen, %d.\n",
	 v, rank, unique_table.nr_items, bdd_nr_frozen_nodes, best_size);
#endif
  } /*for i*/

  alive_after = unique_table.nr_items;

  if (bdd_verbose)
    fprintf (stderr, "done (%d nodes freed, %d vars).\n",
	     alive_before - alive_after, i);
/*
  if (10 * (alive_before - alive_after) < alive_after) {
    bdd_do_dynamic_ordering = 0;
  }
*/
  BDD_FREE_ARRAY (varids, size, int);

  return alive_before - alive_after;
}

/* Reorders var_id to immediately FOLLOW target_var_id in rank (assumes
   dynamic variable ordering is on).
   To make var_id occur as first variable in the order, supply the
   same argument twice, i.e., var_id = target_var_id.
   Does the reordering irrespective of the size of the resultant BDDs
   (worst-case a doubling in size is possible?).
   It is recommended to use this function to set-up an initial variable order
   before any large bdds are built, or to occasionally adjust the order for
   a few crucial variables. In general it is better to rely upon dynamic
   variable ordering.
   DISREGARDS ANY EXISTING GROUPING OF VARIABLES!
   Returns 1 upon success.
   Only in very rare cases involving don't care nodes (BDD_X) the requested
   reordering cannot be established; then 0 is returned and the rank of var_id
   is the closest possible to target_var_id that can be obtained.
*/
int bdd_reorder_var (int var_id, int target_var_id)
{
  int target_rank = BDD_VAR_RANK (target_var_id);
  int        rank = BDD_VAR_RANK (var_id);
  int     success = 1;
  int alive_before;
  /* Threshold factor in increase of best size beyond which swapping is
     stopped. This bounds any hill-climbing.
     For now allow 5 % increase in size, i.e., number of BDD nodes.
  */
  static float THRESHOLD = 1.05;

  if (rank == target_rank + 1)
    /* Already at requested position. */
    return 1;

  /* Cannot proceed unless dynamic ordering is allowed. */
  if (!bdd_do_dynamic_ordering)
    return 0;

  /* Cannot proceed when dead nodes are present! */
  if (bdd_nr_dead_nodes)
    bdd_gc_aux ();

  alive_before = unique_table.nr_items;

  if (rank < target_rank) {
    /* Must swap var_id downwards. */
    while (rank < target_rank) {
      if (rank_swap_impossible (rank)) {
	success = 0;
	break;
      }

      BDD_SWAP_RANKS (rank);
      rank++;

      if (unique_table.nr_items > THRESHOLD * alive_before) {
	success = 0;
	break;
      }
    }
  }
  else {			/* rank >= target_rank */
    if (rank == target_rank)
      /* var_id must become first in order. */
      target_rank = -1;

    /* Must swap var_id upwards. */
    while (rank - 1 > target_rank) {
      if (rank_swap_impossible (rank-1)) {
	success = 0;
	break;
      }

      rank--;
      BDD_SWAP_RANKS (rank);

      if (unique_table.nr_items > THRESHOLD * alive_before) {
	success = 0;
	break;
      }
    }
  }

  /* Can no longer rely on data held in computed table:
     (local_gc most likely has occurred
     and freed nodes are put back to use again)
  */
  bdd_clear_computed_table ();

  return success;
}

/* Moves the orig_rank variables to become the target_rank vars.
   Pre: 0 <= orig_rank <= target_rank <= CURR_MAXRANK
*/
static int bdd_move_rank_down (int orig_rank, int target_rank)
{
  int rank;

  for (rank = orig_rank; rank < target_rank; rank++) {
    if (rank_swap_impossible (rank)) {
      /* Restore to original rank: */
      while (--rank >= orig_rank)
	BDD_SWAP_RANKS (rank);
      return 0;
    }
    BDD_SWAP_RANKS (rank);
  }
  /* rank = target_rank */
  return 1;
}

/* Moves the orig_rank variables to become the target_rank vars.
   Pre: 0 <= target_rank <= rank <= CURR_MAXRANK
*/
static void bdd_move_rank_up (int rank, int target_rank)
{
  while (--rank >= target_rank)
    BDD_SWAP_RANKS (rank);
}

/* Swaps a group of variables with its next group.
   Pre: group and next group exist.
*/
static int bdd_swap_group_down (int g)
{
  int start_rank1 = GROUP_FIRST_RANK(g);
  int   end_rank1 = GROUP_LAST_RANK(g);
  int   nr_ranks2 = GROUP_NR_RANKS(g+1);
  int i, orderable;

  for (i = end_rank1; i >= start_rank1; i--)
    if (!bdd_move_rank_down (i, i + nr_ranks2)) {
      /* Restore the group: */
      while (++i <= end_rank1)
	bdd_move_rank_up (i + nr_ranks2, i);
      return 0;
    }

  /* Reflect swap in group administration: */
  orderable = GROUP_ORDERABLE(g);
  GROUP_ORDERABLE(g)   = GROUP_ORDERABLE(g+1);
  GROUP_ORDERABLE(g+1) = orderable;
  GROUP_LAST_RANK(g)   = start_rank1 + nr_ranks2 - 1;
  return 1;
}

/* Swaps a group of variables with its previous group.
   Pre: (possibly empty) group g and (possibly empty) previous group exists.
*/
#define bdd_swap_group_up(g)	bdd_swap_group_down((g)-1)

/* Count the number of variable occurrences per group: */
static int group_count_items (int g)
{
  int nr_items = 0;
  int start_rank = GROUP_FIRST_RANK(g);
  int end_rank   = GROUP_LAST_RANK(g);

  for (; start_rank <= end_rank; start_rank++)
    nr_items += BDD_VUT_ITEMS(unique_table.space[start_rank]);
  return nr_items;
}

static int *group_nr_items;

static int group_comp (const int *p, const int *q)
{
  return group_nr_items[*q] - group_nr_items[*p];
}

/* Returns number of BDD nodes gained, i.e., the number of alive nodes at
   the start minus the number of alive nodes we end up with.
   Could in some very unlikely cases result in a negative number. Then
   probably some nodes got frozen in the meantime.
*/
int bdd_dynamic_order (void)
{
  int bdd_nodes_gained = 0;
  int *groups;
  int *backrefs;
  int g, i, k;
  int alive_before, alive_after;
  int total = 0;
  /* Threshold factor in increase of best size beyond which swapping is
     stopped. This bounds any hill-climbing.
     For now allow 5 % increase in size, i.e., number of BDD nodes.
  */
  static float THRESHOLD = 1.05;

  if (!bdd_do_dynamic_ordering || !NR_GROUPS)
    return 0;

  bdd_nr_dynamic++;

  /* Cannot do DVO when dead nodes are present! */
  if (bdd_nr_dead_nodes)
    bdd_gc_aux ();

  /* Reorder the variables per group: */
  for (g = 0; g < NR_GROUPS; g++)
    if (GROUP_ORDERABLE(g))
      bdd_nodes_gained += bdd_dynamic_order_group (GROUP_FIRST_RANK(g),
						   GROUP_LAST_RANK(g));

  /* Reorder the groups: */

  alive_before = unique_table.nr_items;

  /* Sort groups according non-increasing number of occurrences of the
     associated BDD nodes BDD_VUT_ITEMS(unique_table.space[rank]).
  */
  check_mem_limit (NR_GROUPS * (2 * sizeof (int)));

  groups         = BDD_MALLOC_ARRAY (NR_GROUPS, int);
  group_nr_items = BDD_MALLOC_ARRAY (NR_GROUPS, int);

  for (g = 0; g < NR_GROUPS; g++) {
    groups[g] = g;
    group_nr_items[g] = group_count_items (g);
  }

  qsort ((void *) groups, (size_t) NR_GROUPS, (size_t) sizeof (int),
	 (int (*) (const void *, const void *)) group_comp);

  /* Find last non-empty group: */
  for (k = NR_GROUPS - 1; k >= 0 && !group_nr_items[groups[k]]; k--);

  BDD_FREE_ARRAY (group_nr_items, NR_GROUPS, int);

  check_mem_limit (NR_GROUPS * sizeof (int));

  backrefs = BDD_MALLOC_ARRAY (NR_GROUPS, int);

  for (g = 0; g < NR_GROUPS; g++)
    backrefs[groups[g]] = g;

  if (bdd_verbose)
    fprintf (stderr,
	     "Dynamic group ordering (%d groups, %d nodes)...",
	     k+1, alive_before);

  /* For each group in order of non-increasing number of occurrences
     as BDD nodes:
  */
  for (i = 0; i <= k; i++) {
    int best_size = unique_table.nr_items;
    int orig_pos  = groups[i];
    int best_pos  = orig_pos;
#if defined(BDD_DEBUG)
    int v = BDD_VUT_ID(unique_table.space[GROUP_LAST_RANK(orig_pos)]);
#endif

    /* Bound the total `time' spent in dynamic ordering.
       Simply stop after a certain number of groups has been considered.
    */
    total += best_size;
    /* 1<<25 =~  34*10^6 */
    /* 1<<28 =~ 268*10^6 */
    /* 1<<30 =~ 10^9 */
    if (total > ((float) (1 << 28)) / (k + 1))
      break;

    g = orig_pos;

#if defined(BDD_DEBUG)
fprintf (stderr, "Orig: v = %3d, rank(v) = %3d, %12d nodes, %4d frozen.\n",
	 v, BDD_VAR_RANK(v), unique_table.nr_items, bdd_nr_frozen_nodes);
#endif

    /* Down. */
    while (g < NR_GROUPS-2) {
      if (!bdd_swap_group_down (g))
	break;

      /* Succeeded swapping groups g and g+1. */
      groups[i]++;
      g++;
      groups[backrefs[g]]--;
      backrefs[g-1] = backrefs[g];
      backrefs[g] = i;

      if (unique_table.nr_items < best_size) {
	best_size = unique_table.nr_items;
	best_pos  = g;
      }
      else
      if (unique_table.nr_items > THRESHOLD * best_size)
	break;
    } /*while*/

#if defined(BDD_DEBUG)
fprintf (stderr, "Down: v = %3d, rank(v) = %3d, %12d nodes, %4d frozen.\n",
	 v, BDD_VAR_RANK(v), unique_table.nr_items, bdd_nr_frozen_nodes);
#endif

    /* Back up to original position: */
    while (g > orig_pos) {
      if (!bdd_swap_group_up (g))
	break;

      /* Succeeded swapping groups g-1 and g. */
      groups[i]--;
      g--;
      groups[backrefs[g]]++;
      backrefs[g+1] = backrefs[g];
      backrefs[g] = i;
    } /*while*/
    /* Here: g == orig_pos */

#if defined(BDD_DEBUG)
fprintf (stderr, "Back: v = %3d, rank(v) = %3d, %12d nodes, %4d frozen.\n",
	 v, BDD_VAR_RANK(v), unique_table.nr_items, bdd_nr_frozen_nodes);
#endif

    /* Up. */
    while (g > 0) {
      if (!bdd_swap_group_up (g))
	break;

      /* Succeeded swapping groups g-1 and g. */
      groups[i]--;
      g--;
      groups[backrefs[g]]++;
      backrefs[g+1] = backrefs[g];
      backrefs[g] = i;

      if (unique_table.nr_items < best_size) {
	best_size = unique_table.nr_items;
	best_pos  = g;
      }
      else
      if (unique_table.nr_items > THRESHOLD * best_size)
	break;
    } /*while*/

#if defined(BDD_DEBUG)
fprintf (stderr, "Up:   v = %3d, rank(v) = %3d, %12d nodes, %4d frozen.\n",
	 v, BDD_VAR_RANK(v), unique_table.nr_items, bdd_nr_frozen_nodes);
#endif

    /* Back down to best position: */
    while (g < best_pos) {
      if (!bdd_swap_group_down (g))
	break;

      /* Succeeded swapping groups g and g+1. */
      groups[i]++;
      g++;
      groups[backrefs[g]]--;
      backrefs[g-1] = backrefs[g];
      backrefs[g] = i;
    } /*while*/
    /* Here: g == best_pos */

#if defined(BDD_DEBUG)
fprintf (stderr, "Best: v = %3d, rank(v) = %3d, %12d nodes, %4d frozen, %d.\n",
	 v, BDD_VAR_RANK(v), unique_table.nr_items, bdd_nr_frozen_nodes,
	 best_size);
#endif

  } /*for i*/

  alive_after = unique_table.nr_items;

  if (bdd_verbose)
    fprintf (stderr, "done (%d nodes freed, %d groups).\n",
	     alive_before - alive_after, i);

  bdd_nodes_gained += alive_before - alive_after;

  BDD_FREE_ARRAY (backrefs, NR_GROUPS, int);
  BDD_FREE_ARRAY (groups,   NR_GROUPS, int);

  /* Can no longer rely on data held in computed table:
     (local_gc has occurred and freed nodes might be put back to use again)
  */
  bdd_clear_computed_table ();

  return bdd_nodes_gained;
}

#if 0
int bdd_dynamic_order (void)
{
  int bdd_nodes_gained;
  int g;

  if (!bdd_do_dynamic_ordering)
    return 0;

  bdd_nr_dynamic++;

  /* Cannot do DVO when dead nodes are present! */
  if (bdd_nr_dead_nodes)
    bdd_gc_aux ();

  bdd_nodes_gained = bdd_dynamic_order_group (0, CURR_MAXRANK);

  /* Can no longer rely on data held in computed table:
     (local_gc has occurred and freed nodes might be put back to use again)
  */
  bdd_clear_computed_table ();

  return bdd_nodes_gained;
}
#endif

/* Check whether "bdd_do_dynamic_ordering" is on, and if so call
   "bdd_dynamic_order" until no more gain is obtained.
*/
void bdd_dynamic_order_exhaustive (void)
{
  if (!bdd_do_dynamic_ordering)
    return;

  if (bdd_verbose)
    fprintf (stderr, "Exhaustive dynamic variable ordering...\n");
  while (bdd_dynamic_order () > 0);
  if (bdd_verbose)
    fprintf (stderr, "Exhaustive dynamic variable ordering...done.\n");
}

/* Makes sure that the subset of variables of which varid is a member, will
   become reorderable during dynamic variable ordering.
   (Singleton groups can never become reorderable though).
*/
void bdd_set_var_group_reorderable (int varid)
{
  int g = bdd_var_id_to_group (varid);

  if (g >= 0)
    GROUP_ORDERABLE(g) = (GROUP_NR_RANKS(g) > (Nat) 1);
}

/* Resets the dynamic reordering flag for the subset of variables of which
   varid is a member. Thus the variables in the group will keep a fixed
   rank order from now on.
*/
void bdd_reset_var_group_reorderable (int varid)
{
  int g = bdd_var_id_to_group (varid);

  if (g >= 0)
    GROUP_ORDERABLE(g) = 0;
}

/* Merges the subsets of variables of which varid_1 and varid_2 are
   representatives. No action when both vars already belong to the same group;
   otherwise this entails a reordering action such that the ranks of the
   new subset will all be consecutive.
   If the original subsets do not have opposite orderability properties,
   then the new group inherits the property, else the new group will become
   not-reorderable.
   Returns 1 when the merge is successful, else 0.
   Failure is due to the incapability of reordering the variables because
   of bdd size restrictions or because it is switched off.
*/
int bdd_merge_var_groups (int varid1, int varid2)
{
  int g1, g2;

  /* Cannot proceed unless dynamic ordering is allowed. */
  if (!bdd_do_dynamic_ordering)
    return 0;

  g1 = bdd_var_id_to_group (varid1);
  g2 = bdd_var_id_to_group (varid2);

  if (g1 < 0 || g2 < 0)		/* invalid group(s) */
    return 0;

  if (g1 == g2)			/* already in same group */
    return 1;

  {
    int g;
    int    size1;
    int    size2;
    int nr_swaps, do_swaps;

    if (g1 > g2) { g = g1; g1 = g2; g2 = g; }
    /* Now: g1 < g2 */

    size1 = GROUP_NR_RANKS(g1);
    size2 = GROUP_NR_RANKS(g2);
    do_swaps = nr_swaps = g2 - g1 - 1;

    /* Cannot proceed when dead nodes are present! */
    if (do_swaps && bdd_nr_dead_nodes)
      bdd_gc_aux ();

    /* Move the smaller group towards the larger one. */
    if (size1 < size2) {
      while (nr_swaps--)
	if (!bdd_swap_group_down (g1++))
	  return 0;
      /* Here: g1 + 1 = g2 */
    }
    else {
      while (nr_swaps--)
	if (!bdd_swap_group_up (g2--))
	  return 0;
      /* Here: g1 + 1 = g2 */
    }
    GROUP_LAST_RANK(g1)  = GROUP_LAST_RANK(g2);
    GROUP_ORDERABLE(g1) &= GROUP_ORDERABLE(g2);

    NR_GROUPS--;
    for (g = g2; g < NR_GROUPS; g++)
      unique_table.groups[g] = unique_table.groups[g+1];

    if (do_swaps)
      /* Can no longer rely on data held in computed table:
	 (local_gc has occurred and freed nodes might be put back to use again)
      */
      bdd_clear_computed_table ();

    return 1;
  }
}

/* Removes all notion of variable grouping: each variable will be put
   in a group by itself.
*/
void bdd_undo_var_groups (void)
{
  int g;

  NR_GROUPS = unique_table.count;
  for (g = 0; g < NR_GROUPS; g++) {
    GROUP_LAST_RANK(g) = g;
    GROUP_ORDERABLE(g) = 0;
  }
}
