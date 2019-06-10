/*
 DOCUMENTATION INFORMATION				         module: BDD
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system	   : HP 9000/S500, IBM RS/6000
 file	   : bdd_factor.h
 unit-title: 
 ref.	   :
 author(s) : Copyright (c) 1994 Jeroen Soede, Koen van Eijk, Geert Janssen
 date	   : 20-OCT-1994
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

/* ************************************************************************ */
/* FILE DOCUMENTATION:                                                      */
/*                                                                          */
/* ************************************************************************ */

#ifndef BDD_FACTOR_H
#define BDD_FACTOR_H

/* ------------------------------------------------------------------------ */
/* DEFINES                                                                  */
/* ------------------------------------------------------------------------ */

/* Encoding for the logical operators used in the expressions.
   Numbers happen to reflect the precedence used by
   the default "handle_left" and "handle_right" routines.
   Any user of course is free to supply his own versions
   of these routines that use a different precedence, e.g.
   by looking up the priorities in a 4-place table.
*/
#define BDD_NO_OP	0
#define BDD_OR_OP	1
#define BDD_XOR_OP	2
#define BDD_AND_OP	3

/* ------------------------------------------------------------------------ */
/* TYPE DEFINITIONS                                                         */
/* ------------------------------------------------------------------------ */

typedef struct {
  FILE *out; /* Pointer to open file used by handlers when printing. */
  void (*handle_const)      (BDDPTR v); /* v can be: void, X, 0 or 1 */
  void (*handle_lit)        (int v, int negated);
  void (*handle_or)         (void);
  void (*handle_and)        (void);
  void (*handle_xor)        (void);
  void (*handle_left)       (int op, int context);
  void (*handle_right)      (int op, int context);
  void (*handle_sub_index)  (int v, int negated);
  void (*handle_root_index) (int v, int negated);
  void (*handle_begin_expr) (int v);
  void (*handle_end_expr)   (void);
  void (*handle_begin_root) (int v);
  void (*handle_end_root)   (void);
  void (*handle_root_exprs) (int count);
  void (*handle_sub_exprs)  (int count);

  int  style;		 /* 1 : Koen's style (default); 0: Geert's style. */
  int  use_xors;         /* 1 : use xors during printing */
  int  use_impl_check;   /* 1 : use implication checks */
  int  maxI;             /* highest index used of factor matrix below;
			    use value -1 for "no subexpressions". */
  
  struct {               /* A defines when a subexpression is created:   */
    int in;              /* If the size of a node >= A[i].limitsize and  */
    int limitsize;       /* #inedges >= A[i].in a subexpression is made. */
  }  A[6];               /* Fixed array length */
} bdd_factor_interface; 

/* ------------------------------------------------------------------------ */
/* VARIABLES                                                                */
/* ------------------------------------------------------------------------ */
//static char SccsId_BDD_FACTOR_H[] = "%Z%%Y%/%M% %I% %G%";

/* ------------------------------------------------------------------------ */
/* FUNCTION PROTOTYPES                                                      */
/* ------------------------------------------------------------------------ */

extern void bdd_factor_vec           (BDDPTR *F, int vec_size);
extern void bdd_factor               (BDDPTR f);
extern bdd_factor_interface *bdd_get_factor_interface (void);
extern void bdd_set_factor_interface (bdd_factor_interface *new_interface);

#endif /* BDD_FACTOR_H */
