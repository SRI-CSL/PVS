/*
 DOCUMENTATION INFORMATION				   module: MU CALCULUS
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system    : Hewlett-Packard HP9000/S750
 file	   : mu.h
 unit-title: 
 ref.	   : 
 author(s) : Copyright (c) 1992-1995 G.L.J.M. Janssen
 date	   : 27-SEP-1995
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

#ifndef MU_H
#define MU_H

#include "alloc.h"
#include "hash.h"

/* ------------------------------------------------------------------------ */
/* DEFINES                                                                  */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* TYPE DEFINITIONS                                                         */
/* ------------------------------------------------------------------------ */

typedef struct _Signature _Signature, *Signature;

typedef struct _Formula _Formula, *Formula;
typedef struct _Term _Term, *Term;
typedef struct _R_Interpret _R_Interpret, *R_Interpret;

typedef enum {
  MU_AND,
  MU_OR,
  MU_COFACTOR,
  MU_IMPLIES,
  MU_EQUIV,
  MU_XOR,

  MU_FALSE,
  MU_TRUE,
  MU_B_VAR,
  MU_NOT,

  MU_ITE,
  MU_EXIST,
  MU_DIFF,
  MU_UNIV,
  MU_APPLY,
  MU_ONE_OF,
  MU_NONE_OF,
  MU_SUBST
} FormulaType;

typedef enum {
  MU_T_AND,
  MU_T_OR,
  MU_T_DUMMY1,
  MU_T_IMPLIES,
  MU_T_EQUIV,
  MU_T_XOR,

  MU_T_FALSE,
  MU_T_TRUE,
  MU_R_VAR,
  MU_T_NOT,

  MU_ABSTRACT,
  MU_L_FIXED_POINT,
  MU_G_FIXED_POINT,
  MU_REACH
} TermType;

/* ------------------------------------------------------------------------ */
/* VARIABLES                                                                */
/* ------------------------------------------------------------------------ */

static char SccsId_MU_H[] = "%Z%%Y%/%M% %I% %G%";

extern Signature signature;
extern R_Interpret      Ip;

extern int mu_simplify_frontier;
extern int mu_use_and_smooth;
extern int mu_use_restrict;
extern int mu_use_constrain;
extern int mu_verbose;
extern int mu_echo;
extern int mu_debug;

/* ------------------------------------------------------------------------ */
/* FUNCTION PROTOTYPES                                                      */
/* ------------------------------------------------------------------------ */

extern void mu_init (void);
extern void mu_quit (void);

extern const char *mu_bool_var_name (int var);

extern  BDDPTR mu_interpret_term    (Term T,    R_Interpret Ip, Term FT);
extern  BDDPTR mu_interpret_formula (Formula f, R_Interpret Ip, Term FT);

extern    void mu_mk_signature (LIST vars);
extern Formula mu_mk_false_formula (void);
extern Formula mu_mk_true_formula (void);
extern Formula mu_mk_bool_var (char *name);
extern Formula mu_mk_unary_formula (FormulaType type, Formula f1);
extern Formula mu_mk_binary_formula (FormulaType type, Formula f1, Formula f2);
extern Formula mu_mk_ite_formula (Formula cond,
				  Formula then_part, Formula else_part);
extern Formula mu_mk_quantified_formula (FormulaType type, LIST vars,
					 Formula f1);
extern Formula mu_mk_subst (Formula f, int var, Formula g);
extern Formula mu_mk_application (Term R, LIST subs, int curried);
extern Formula mu_mk_one_of (LIST subs);
extern Formula mu_mk_none_of (LIST subs);
extern Formula mu_BDD_2_Formula (BDDPTR f);

extern    Term mu_mk_false_term (void);
extern    Term mu_mk_true_term (void);
extern    Term mu_mk_rel_var (R_Interpret Ip, char *name);
extern     int mu_mk_rel_var_dcl (char *name);
extern    Term mu_mk_abstraction (LIST vars, Formula f1);
extern    Term mu_mk_curry (Formula FT);
extern    Term mu_mk_fixed_point (TermType type, R_Interpret Ip, int var,
				  Term R, unsigned int iter_bound);
extern    Term mu_mk_reach (Term Next, Term S0, Term Inv);
extern    Term mu_mk_unary_term (TermType type, Term T1);
extern    Term mu_mk_binary_term (TermType type, Term T1, Term T2);
extern    void mu_mk_let (int var, Term T);
extern    Term mu_BDD_2_Term (BDDPTR f);

extern  int mu_check_bool_var (char *name);
extern void mu_free_formula (Formula f);
extern void mu_free_term (Term T);

extern void mu_print_formula_infix (FILE *fp, Formula f);
extern void mu_print_term_infix (FILE *fp, Term t);

#endif /* MU_H */
