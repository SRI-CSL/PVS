/*
 DOCUMENTATION INFORMATION				         module: BDD
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system	   : Apollo DN3000, HP 9000/S500, IBM RS/6000
 file	   : bdd_extern.h
 unit-title: 
 ref.	   : Efficient Implementation of a BDD Package, Karl S. Brace. DAC'90
 author(s) : Copyright (c) 1990-1997 G.L.J.M. Janssen
 date	   : 18-NOV-1997
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

#ifndef BDD_EXTERN_H
#define BDD_EXTERN_H

/* ************************************************************************ */
/* FILE DOCUMENTATION:                                                      */
/* Minimum necessary extern definitions of variables and functions to use   */
/* the BDD package. For more control bdd.h should be included, which will   */
/* automatically include this file too.                                     */
/* ************************************************************************ */

/* ------------------------------------------------------------------------ */
/* INCLUDES                                                                  */
/* ------------------------------------------------------------------------ */

#include <stdio.h>

/* ------------------------------------------------------------------------ */
/* DEFINES                                                                  */
/* ------------------------------------------------------------------------ */

/* The void BDD: */
#define BDD_VOID		((BDDPTR) 0)

/* ------------------------------------------------------------------------ */
/* TYPE DEFINITIONS                                                         */
/* ------------------------------------------------------------------------ */

typedef struct bdd *BDDPTR;

/* ------------------------------------------------------------------------ */
/* VARIABLES                                                                */
/* ------------------------------------------------------------------------ */
static char SccsId_BDD_EXTERN_H[] = "%Z%%Y%/%M% %I% %G%";

/* The 1-function: */
extern /*const*/ BDDPTR BDD_1;

/* The 0-function: */
extern /*const*/ BDDPTR BDD_0;

/* The dontcare-function: */
extern /*const*/ BDDPTR BDD_X;

/* User settable program parameters: */
extern int bdd_do_dynamic_ordering;	/* default 1 */
extern int bdd_do_gc;			/* default 1 */
extern int bdd_verbose;			/* default 0 */
extern int bdd_debug;			/* default 0 */
extern int bdd_use_neg_edges;		/* default 1 */
extern int bdd_use_inv_edges;		/* default 1 */
extern int bdd_ok_to_use_MSB;		/* default 0 */
extern int bdd_sizeof_user_data;	/* default 0 */
extern int bdd_nr_nodes_allowed;	/* default INT_MAX */

/* Initial sizes of tables: */
extern int BDD_COMPUTED_TABLE_SIZE; /* DEFAULT_BDD_COMPUTED_TABLE_SIZE */
extern int BDD_HASHTAB_SIZE;	    /* DEFAULT_BDD_HASHTAB_SIZE */
extern int BDD_NR_RANKS;	    /* DEFAULT_BDD_NR_RANKS */

extern int BDD_LOAD_FACTOR;	    /* DEFAULT_BDD_LOAD_FACTOR */

/* Considered to be user read-only: */
extern int bdd_nr_vars;
extern int bdd_nr_dead_nodes;
extern int bdd_nr_frozen_nodes;
extern int bdd_nr_gc;
extern int bdd_nr_dynamic;
extern int bdd_peak_nr_nodes;
extern int bdd_peak_nr_nodes_alive;

/* ------------------------------------------------------------------------ */
/* FUNCTION PROTOTYPES                                                      */
/* ------------------------------------------------------------------------ */

/* Package administration related routines: */
extern   void bdd_init          (void);
extern   void bdd_quit          (void);
extern   void bdd_print_stats   (FILE *fp);
extern    int bdd_gc            (void);
extern    int bdd_dynamic_order (void);
extern   void bdd_dynamic_order_exhaustive (void);
extern    int bdd_reorder_var   (int var_id, int target_var_id);
extern    int bdd_memsize       (void);
extern    int bdd_memsize_limit (void);
extern    int bdd_top_memsize   (void);
extern   void bdd_set_memsize_limit_and_handler
                (int limit, void (*handler) (void));
extern   void bdd_set_gc_hook	(void (*hook) (void));
extern    int bdd_nodes_alive   (void);
extern    int bdd_nodes_allocated (void);
extern    int bdd_nr_occurs_var (int id);

/* Constant BDD creators: */
extern BDDPTR bdd_0             (void);
extern BDDPTR bdd_1             (void);
extern BDDPTR bdd_X             (void);

/*extern BDDPTR bdd_create_user_terminal (void *data);*/

/* Variable creators: */
extern BDDPTR bdd_create_var		(int v);
extern BDDPTR bdd_create_var_first	(void);
extern BDDPTR bdd_create_var_before	(BDDPTR v);
extern BDDPTR bdd_create_var_after	(BDDPTR v);
extern BDDPTR bdd_create_var_last	(void);

/* Control over variable grouping: */
extern void bdd_set_var_group_reorderable	(int varid);
extern void bdd_reset_var_group_reorderable	(int varid);
extern  int bdd_merge_var_groups		(int varid1, int varid2);
extern void bdd_undo_var_groups			(void);

/* Predicates on BDDs */
extern    int bdd_void_p        (BDDPTR f);
extern    int bdd_const_p       (BDDPTR f);
extern    int bdd_literal_p     (BDDPTR f);
extern    int bdd_poslit_p      (BDDPTR f);
extern    int bdd_neglit_p      (BDDPTR f);
extern    int bdd_equal_p       (BDDPTR f, BDDPTR g);
extern    int bdd_compl_p       (BDDPTR f, BDDPTR g);
extern    int bdd_frozen_p      (BDDPTR f);

/* Logic operators: */
extern BDDPTR bdd_ite           (BDDPTR F, BDDPTR G, BDDPTR H);
extern BDDPTR bdd_ite_const     (BDDPTR F, BDDPTR G, BDDPTR H);
extern BDDPTR bdd_assign        (BDDPTR f);
extern BDDPTR bdd_not           (BDDPTR f);
extern BDDPTR bdd_and           (BDDPTR f, BDDPTR g);
extern BDDPTR bdd_greater       (BDDPTR f, BDDPTR g);
extern BDDPTR bdd_less          (BDDPTR f, BDDPTR g);
extern BDDPTR bdd_xor           (BDDPTR f, BDDPTR g);
extern BDDPTR bdd_or            (BDDPTR f, BDDPTR g);
extern BDDPTR bdd_nor           (BDDPTR f, BDDPTR g);
extern BDDPTR bdd_equiv         (BDDPTR f, BDDPTR g);
extern BDDPTR bdd_xnor          (BDDPTR f, BDDPTR g);
extern BDDPTR bdd_implied       (BDDPTR f, BDDPTR g);
extern BDDPTR bdd_implies       (BDDPTR f, BDDPTR g);
extern    int bdd_implies_taut  (BDDPTR f, BDDPTR g);
extern BDDPTR bdd_nand          (BDDPTR f, BDDPTR g);
extern BDDPTR bdd_invert_input_top (BDDPTR f);
extern BDDPTR bdd_then          (BDDPTR f);
extern BDDPTR bdd_else          (BDDPTR f);
extern BDDPTR bdd_constrain	(BDDPTR f, BDDPTR c);
extern BDDPTR bdd_restrict	(BDDPTR f, BDDPTR c);

/* Destructors: */
extern   void bdd_free          (BDDPTR f);
extern BDDPTR bdd_freeze        (BDDPTR f);
extern   void bdd_free_vec      (BDDPTR *f_vec, int size);

/* BDD Inquiries: */
extern BDDPTR bdd_top_var       (BDDPTR f);
extern    int bdd_top_var_id    (BDDPTR f);
extern    int bdd_top_var_rank  (BDDPTR f);
extern    int bdd_size          (BDDPTR f);
extern    int bdd_depth         (BDDPTR f);
extern    int bdd_size_vec      (BDDPTR *f_vec, int size);
extern    int bdd_size_ceil     (BDDPTR f, int ceiling);
extern    int bdd_max_var_rank  (BDDPTR f);

/* Miscellaneous functions: */
extern   void bdd_print_node	(FILE *fp, BDDPTR v);
extern   void bdd_print         (FILE *fp, BDDPTR f, char *s);
extern    int bdd_var_rank_to_id(int rank);
extern    int bdd_var_id_to_rank(int id);
extern   void bdd_cofactors	(BDDPTR f, BDDPTR *vp, BDDPTR *Tp, BDDPTR *Ep);
extern    int bdd_valid_p       (BDDPTR f);
extern    int bdd_check_valid   (BDDPTR f, char *text);
extern  const char *bdd_invalid_reason (int reason);
/*
extern   void bdd_dump_to_file_vec (FILE *fp, BDDPTR *f_vec, int size);
extern BDDPTR *bdd_restore_from_file_vec (FILE *fp, BDDPTR *f_vec, int *len);
*/
#ifndef BDD_LIGHT
extern unsigned char *bdd_dump_to_chars_vec (BDDPTR *f_vec, int size);
extern BDDPTR *bdd_restore_from_chars_vec (unsigned char *b, BDDPTR *f_vec, int *len);
extern   void bdd_free_dumped_chars (unsigned char *b);
#endif
extern   void bdd_null_action   (BDDPTR f);
extern   void bdd_traverse_pre  (BDDPTR f, void (*pre_action)(BDDPTR));
extern   void bdd_traverse_pre_rec  (BDDPTR f, void (*pre_action) (BDDPTR));
extern   void bdd_traverse_post (BDDPTR f, void (*post_action)(BDDPTR));
extern   void bdd_traverse_post_rec (BDDPTR f, void (*post_action) (BDDPTR));
extern   void bdd_reset_marks   (BDDPTR f);
extern BDDPTR bdd_apply		(BDDPTR (*f)(BDDPTR,BDDPTR),BDDPTR a,BDDPTR b);

#endif /* BDD_EXTERN_H */
