/*
 DOCUMENTATION INFORMATION				         module: BDD
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system	   : Apollo DN3000, HP 9000/S500
 file	   : plot.c
 unit-title: 
 ref.	   : 
 author(s) : Copyright (c) 1991-1996 G.L.J.M. Janssen/ E. Huijbregts
 date	   : 19-NOV-1996
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

/* ------------------------------------------------------------------------ */
/* INCLUDES                                                                 */
/* ------------------------------------------------------------------------ */

#include <stdio.h>
#include <stdlib.h>

#include "bdd.h"
#include "plot.h"

/* ------------------------------------------------------------------------ */
/* LOCAL DEFINES                                                            */
/* ------------------------------------------------------------------------ */

#define min(a,b)	((a) < (b) ? (a) : (b))
#define max(a,b)	((a) > (b) ? (a) : (b))

/* ------------------------------------------------------------------------ */
/* LOCAL TYPE DEFINITIONS                                                   */
/* ------------------------------------------------------------------------ */


/* ------------------------------------------------------------------------ */
/* LOCAL VARIABLES                                                          */
/* ------------------------------------------------------------------------ */

static int max_level;
static int min_delta;
static int margin;
static int max_pos;

/* ------------------------------------------------------------------------ */
/* LOCAL FUNCTION PROTOTYPES                                                */
/* ------------------------------------------------------------------------ */


/* ************************************************************************ */
/* FUNCTION DOCUMENTATION:                                                  */
/*                                                                          */
/* ************************************************************************ */

/* BDD nodes are arranged on levels. Level numbers are assigned starting
   with level 1 for the top variable; The constant nodes will always be
   on level `max_level' (>= 1).

   Two numbers are associated to each BDD node:
   its x position (increasing from left to right), and
   its level or y position (increasing from top to bottom).
*/

/* Initialize the coordinate fields of a BDD node. */
static void init_action (BDDPTR v)
{
  /* Constant nodes inclusive! */
  POSITION_F (v) = 0;
  LEVEL_F    (v) = 0;
}

/* Count the incoming edges per BDD node (constants inclusive):
   This looks a lot like recalculating the reference count for a node,
   however this case is more restricted since we only consider
   references caused by certain root nodes.
*/
static void count_inedges_action (BDDPTR v)
{
  /* (Mis)use the position field to temp. hold the number of incoming edges. */
  if (!BDD_TERM_P (v)) {
    POSITION_F (BDD_THEN (v))++;
    POSITION_F (BDD_ELSE (v))++;
  }
  /* else a constant has no children. */
}

/* Assign level numbers to nodes. v could be a constant.
   This algorithm does an ASAP leveling, i.e., it pulls up the nodes as
   far as possible to the root nodes.
   Assumes number of incoming edges is recorded in POSITION field.
   All POSITIONs > 0.
   Assigns result to LEVEL field and calculates max_level.
*/
static void levelize1 (BDDPTR v,
		       int level) /* next level number to assign to a node */
{
 restart:
  /* Decrement number of incoming edges: */
  POSITION_F (v)--;

  /* Assign max level of predecessor: */
  if (level > LEVEL_F (v))
    LEVEL_F (v) = level;

  /* POSITION_F (v) == 0 means this is the last time we see this node. */

  if (!POSITION_F (v) && !BDD_TERM_P (v)) {
    /* All higher level nodes seen and this node is not constant. */
    level = LEVEL_F (v) + 1;
    max_level = max (max_level, level);
    /* Recurse: */
    levelize1 (BDD_THEN (v), level);
    /* Avoid tail-recursion: levelize1 (BDD_ELSE (v), level); */
    v = BDD_ELSE (v);
    goto restart;
  }
}

/* Assign level numbers to nodes. v could be a constant.
   This algorithm does a leveling based on the variable ranks, i.e., it will
   put all variables of the same rank on one level. Missing variables will
   still occupy a level; no compaction in the Y direction done yet!
   Assumes number of incoming edges is recorded in POSITION field.
   All POSITIONs > 0.
   Assigns result to LEVEL field and calculates max_level.
*/
static void levelize2 (BDDPTR v, int level /* dummy argument */)
{
 restart:
  /* Decrement number of incoming edges: */
  POSITION_F (v)--;

  /* POSITION_F (v) == 0 means this is the last time we see this node. */

  if (!POSITION_F (v) && !BDD_TERM_P (v)) {
    LEVEL_F (v) = BDD_RANK (v) + 1;
    max_level = max (max_level, LEVEL_F (v));
    /* Recurse: */
    levelize2 (BDD_THEN (v), level);
    /* Avoid tail-recursion: levelize2 (BDD_ELSE (v), level); */
    v = BDD_ELSE (v);
    goto restart;
  }
}

/* Don't use traverse_pre since we like a real recursive procedure here
   to control the delta value.
*/
static void assign_pos (BDDPTR v, int delta)
{
 restart:
  BDD_TOGGLE_MARK (v);

  /* Disregard constant nodes: */
  if (!BDD_TERM_P (v)) {
    BDDPTR T = BDD_THEN (v);
    BDDPTR E = BDD_ELSE (v);
    BDDPTR ME;
    int count = 0;

    /* Count number of distinct unplaced children on next level: */
    if (BDD_NOT_MARKED (v, T) && LEVEL_F (T) == LEVEL_F (v) + 1) {
      count++;
      ME = T;
      /* Mark, thus avoiding to count the same child more often: */
      POSITION_F (T) = 1;
    }
    /* Don't use "else" because T and E might point to the same node. */
    if (BDD_NOT_MARKED (v, E) && LEVEL_F (E) == LEVEL_F (v) + 1
	&& !POSITION_F (E)) {
      count++;
      ME = E;
    }

    /* 0 <= count <= 2 */
    switch (count) {
    case 0:
      /* Don't place the child. */
      break;

    case 1:
      /* Place the child on the same position as its father node: */
      POSITION_F (ME) = POSITION_F (v);
      /* Avoid tail-recursion: assign_pos (ME, delta);*/
      v = ME;
      goto restart;

    case 2:
      /* Place the children delta to the left and right of the father: */
      POSITION_F (T) = POSITION_F (v) - delta;
      POSITION_F (E) = POSITION_F (v) + delta;

      margin = min (margin, POSITION_F (T));  

      /* Divide delta by 2 with lowerbound 0: */
      if (delta) delta >>= 1;
      min_delta = min (min_delta, delta);
      assign_pos (T, delta);
      /* Avoid tail-recursion: assign_pos (E, delta);*/
      v = E;
      goto restart;
    }
  }
  /*else v is a constant: no x position assigned. */
}

static void margin_correction_action (BDDPTR v)
{
  if (!BDD_TERM_P (v))
    POSITION_F (v) -= margin;
}

static void delta_correction_action (BDDPTR v)
{
  if (!BDD_TERM_P (v)) {
    POSITION_F (v) /= min_delta;
    max_pos         = max (max_pos, POSITION_F (v));
  }
}

static int pos_counter;
static void count_pos_action (BDDPTR v)
{
  if (!BDD_TERM_P (v))
    pos_counter++;
}

static BDDPTR *collection;
static void collect_pos_action (BDDPTR v)
{
  if (!BDD_TERM_P (v))
    collection[pos_counter++] = v;
}

static int pos_less_than (BDDPTR *v1, BDDPTR *v2)
{
  return POSITION_F (*v1) - POSITION_F (*v2);
}

static int level_less_than (BDDPTR *v1, BDDPTR *v2)
{
  return LEVEL_F (*v1) - LEVEL_F (*v2);
}

void bdd_prepare_for_plot_vec (BDDPTR *f_vec, int size,
			       int *max_xp, int *max_yp,
			       int compact,
			       int use_rank_leveling)
{
  register int i;

  if (!f_vec || size <= 0) {
    *max_xp = 0;
    *max_yp = 0;
    return;
  }

  /* Assumes all mark fields are 0: */

  /* (Re)initialize all POSITION and LEVEL fields (TERMs inclusive): */
  for (i = 0; i < size; i++) {
    BDDPTR f = f_vec[i];

    /* Note: vector of BDD's could share vertices. */
    if (!BDD_VOID_P (f) && !BDD_MARK (f))
      bdd_traverse_pre (f, init_action);
  }
  /* Now all mark fields are set. */

  /* Record #incoming edges per node in POSITION field (TERMs inclusive): */
  for (i = 0; i < size; i++) {
    BDDPTR f = f_vec[i];

    if (!BDD_VOID_P (f)) {
      POSITION_F (f)++;		/* Count 1 for occurrence of top node. */

      if (BDD_MARK (f))
	bdd_traverse_pre (f, count_inedges_action);
    }
  }
  /* Now all mark fields are 0 again. */
  /* All POSITION fields are > 0. */

  /* Levelize the nodes of the BDD (TERMs inclusive): */
  /* Levels are counted from 1; constant nodes are on max_level. */
  max_level = 1;
  for (i = 0; i < size; i++) {
    BDDPTR f = f_vec[i];

    if (!BDD_VOID_P (f)) {
      /* Must start levelize only on top nodes with just 1 incoming edge: */
      if (POSITION_F (f) == 1)
	/* Level of top node will be 1: */
	(use_rank_leveling ? levelize2 : levelize1) (f, 1);
      else
	/* Decrement to indicate we dealt with this reference. */
	POSITION_F (f)--;
    }
  }
  /* All POSITION fields are 0. */

  if (use_rank_leveling) { /* Must compact the levels! */
    int nr_nodes;

    /* Count all the nodes: */
    pos_counter = 0;
    for (i = 0; i < size; i++) {
      BDDPTR f = f_vec[i];

      if (!BDD_VOID_P (f) && !BDD_MARK (f))
	bdd_traverse_pre (f, count_pos_action);
    }
    nr_nodes = pos_counter;                  /* perhaps 0 */
    /* Now all mark fields are 1 again. */

    /* Collect all BDD nodes: */
    collection = nr_nodes ? (BDDPTR *) malloc (nr_nodes * sizeof (BDDPTR))
                          : NULL;
    pos_counter = 0;
    for (i = 0; i < size; i++) {
      BDDPTR f = f_vec[i];

      if (!BDD_VOID_P (f) && BDD_MARK (f))
	bdd_traverse_pre (f, collect_pos_action);
    }
    /* Now all mark fields are 0 again. */

    /* Sort BDD nodes in collection on LEVEL: */
    qsort ((void *) collection, (size_t) nr_nodes,
	   (size_t) sizeof (BDDPTR),
	   (int (*) (const void *, const void *)) level_less_than);

    /* Assign new consecutive levels starting from 1.
       Keep nodes that originally had same level on same new level.
    */
    {
      int new_level = 0;
      int orig_level = -1;

      for (i = 0; i < nr_nodes; i++) {
	BDDPTR v = collection[i];

	if (LEVEL_F (v) != orig_level) {
	  orig_level = LEVEL_F (v);
	  new_level++;
	}
	LEVEL_F (v) = new_level;
      }
      max_level = new_level;
    }
    if (collection) free (collection);
  }
  /* Here max_level is known. */
  /* Note: all mark fields are still 0. */

  /* Distribute top variables along the X-axis stretch 0..2^31-1: */

  margin = (1 << 30) / size;

  /* Children will be delta to the left and right of f: */
  min_delta = (1 << 29) / size;
  /* Assign X position to each node: */
  for (i = 0; i < size; i++) {
    BDDPTR f = f_vec[i];

    /* Must start assigning position to nodes on level 1: */
    if (!BDD_VOID_P (f) && !BDD_MARK (f) && LEVEL_F (f) == 1) {
      /* This might assign a position to a constant, but
	 that value is not very useful.
      */
      POSITION_F (f) = ((i << 1) + 1) * ((1 << 30) / size);
      assign_pos (f, (1 << 29) / size);
    }
  }
  /* Here min_delta and margin are known. */
  /* Now all mark fields are 1 again. */

  /* Correct X pos for minimum delta, also correct for margin
     and determine max_pos:
  */
  if (!min_delta)
    min_delta = 1;
  else
    min_delta <<= 1;

  for (i = 0; i < size; i++) {
    BDDPTR f = f_vec[i];

    if (!BDD_VOID_P (f) && BDD_MARK (f))
      bdd_traverse_pre (f, margin_correction_action);
  }
  /* Now all mark fields are 0 again. */

  max_pos = 0;

  for (i = 0; i < size; i++) {
    BDDPTR f = f_vec[i];

    if (!BDD_VOID_P (f) && !BDD_MARK (f))
      bdd_traverse_pre (f, delta_correction_action);
  }
  /* Here max_pos is known. */
  /* Now all mark fields are 1 again. */

  for (i = 0; i < size; i++) {
    BDDPTR f = f_vec[i];

    if (!BDD_VOID_P (f) && BDD_MARK (f))
      bdd_reset_marks (f);
  }
  /* Now all mark fields are 0 again. */

  if (compact) {
    /* Compact the graph in horizontal direction: */
    int nr_nodes;

    /* Count all relevant X-positions: */
    pos_counter = 0;
    for (i = 0; i < size; i++) {
      BDDPTR f = f_vec[i];

      if (!BDD_VOID_P (f) && !BDD_MARK (f))
	bdd_traverse_pre (f, count_pos_action);
    }

    nr_nodes = pos_counter;                  /* perhaps 0 */
    /* Now all mark fields are 1 again. */

    /* Collect all BDD nodes with relevant X-positions: */
    collection = nr_nodes ? (BDDPTR *) malloc (nr_nodes * sizeof (BDDPTR))
                          : NULL;
    pos_counter = 0;
    for (i = 0; i < size; i++) {
      BDDPTR f = f_vec[i];

      if (!BDD_VOID_P (f) && BDD_MARK (f))
	bdd_traverse_pre (f, collect_pos_action);
    }
    /* Now all mark fields are 0 again. */

    /* Sort BDD nodes in collection on POSITION: */
    qsort ((void *) collection, (size_t) nr_nodes,
	   (size_t) sizeof (BDDPTR),
	   (int (*) (const void *, const void *)) pos_less_than);

    /* Assign new consecutive positions starting from 0.
       Keep nodes that originally had same position on same new position.
    */
    {
      int new_pos = -1;
      int orig_pos = -1;

      for (i = 0; i < nr_nodes; i++) {
	BDDPTR v = collection[i];

	if (POSITION_F (v) != orig_pos) {
	  orig_pos = POSITION_F (v);
	  new_pos++;
	}
	POSITION_F (v) = new_pos;
      }
      max_pos = new_pos;
    }

    if (collection) free (collection);
  }
  *max_xp = max_pos + 1;
  *max_yp = max_level;
}
