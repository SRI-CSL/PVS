%{
/*
 DOCUMENTATION INFORMATION                               module: BDD EXAMPLE
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system    : Apollo DN3000, IBM RS/6000
 file      : yacc.y
 unit-title: YACC GRAMMAR RULES FOR BDD INPUT
 ref.      : 
 author(s) : Copyright (c) 1990-1998 G.L.J.M. Janssen
 date      : 27-MAR-1998
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/
%}

/* The Reserved Words: */
%token LET_SYM
%token VOID_SYM
%token TRUE_SYM
%token FALSE_SYM
%token DONTCARE_SYM
%token WHILE_SYM
%token TRANS_SYM

/* Identifier and Constants: */
%token IDENTIFIER
%token STRING

/* Operators: */
%token NOT_SYM
%token POSTFIX_NOT_SYM
%token AND_SYM
%token OR_SYM
%token IMPLIES_SYM
%token IMPLIED_BY_SYM
%token EQUIV_SYM
%token XOR_SYM

/* Predefined Functions: */
%token IN_SYM
%token CUT_SYM
%token C2V_SYM
%token V2C_SYM
%token SUPPORT_SYM
%token INV_INPUTS_SYM
%token INV_INPUT_SYM
%token SUBST_SYM
%token SUBST2_SYM
%token HAS_X_SYM
%token ONE_OF_SYM
%token MINIMIZE_SYM
%token MINIMIZE_X_SYM
%token TOP_SYM
%token CUBE_SYM
%token ON_SET_SYM
%token OFF_SET_SYM
%token X_SET_SYM
%token REPLACE_X_SYM
%token INTERSECT_SYM
%token SUBSET_SYM
%token REPLACE_C_SYM
%token EVAL_SYM
%token ITE_SYM
%token ITEC_SYM
%token FREE_SYM
%token FREEZE_SYM
%token UNATE_IN_SYM
%token SET_SYM
%token WRITE_SYM
%token GC_SYM
%token PATH_SYM
%token MTERMS_SYM
%token CORE_SYM
%token PRIME_SYM
%token REORDER_SYM
%token SWAP_SYM

/* Punctuation: */
%token ASSIGN
%token A_SYM
%token E_SYM
%token D_SYM

%{
#include <stdio.h>
#include <stdarg.h>
#include "alloc.h"
#include "hash.h"
#include "bdd_fns.h"
#include "bdd_vfns.h"
#include "appl.h"

extern char yytext[];
extern int yyleng;
extern int yylineno;

extern int warnings;
extern char *filename;

extern void action (BDDPTR *F);
extern void parse_complete (void);

void yyerror (char *format, ...);
void yywarning (char *format, ...);

#ifdef COMMENT
static void print_int (FILE *fp, void *cont)
{
  fprintf (fp, "%d", (int) cont);
}
#endif

static BDDPTR trans_closure (BDDPTR N, BDD_LIST x, BDD_LIST y)
{
  yyerror ("Not implemented.");
  return BDD_VOID;
}

/* Precisely one of args true. */
static BDDPTR mk_one_of (BDD_LIST args)
{
  BDDPTR R = bdd_one_of_list (args);

  bdd_list_free (args, (void (*)(void *)) bdd_free);

  return R;
}

%}

%union
{
  int ival;
  char *str;
  BDD_LIST list;
  BDDPTR bdd;
  BDDPTR *bddvec;
}

%type <str>	IDENTIFIER
%type <str>	STRING
%type <bdd>	Rule Head Body Expr Identity PL_formula PL_formula_1
%type <bdd>	formula term cofactor factor primary atomic_formula variable
%type <bdd>	Def_Expr
%type <list>	arg_list
%type <ival>	identifier
%type <bddvec>	Vector
%type <list>	Elements

%start PL_file

%%
PL_file :
;
PL_file : PL_input
{
  parse_complete ();
}
;

PL_input : Rule_or_Statement '.'
{
}
;
PL_input : PL_input Rule_or_Statement '.'
{
}
;
PL_input : Rule_or_Statement ';'
{
}
;
PL_input : PL_input Rule_or_Statement ';'
{
}
;

Rule_or_Statement : Rule
{
  BDDPTR *F = MakeBDDVec (1);

  F[0] = $1;
  action (F);
}
;
Rule_or_Statement : Vector
{
  action ($1);
}
;
Rule_or_Statement : Statement
;

Vector : '[' Elements ']'
{
  BDDPTR *F, *p;
  BDD_LIST list = $2;
  int size = BDD_LIST_SIZE (list);

  F = p = MakeBDDVec (size);
  /* First in list is MSB. */
  BDD_LIST_FOR_EACH_ELEM (list, elem) {
    BDDPTR f = BDD_ELEM_CONTENTS (elem);

    *p++ = f;
  } BDD_LIST_END_FOR_EACH_ELEM;
  bdd_list_free (list, 0);
  $$ = F;
}
;
Elements : Expr
{
  $$ = bdd_list_push_cont ($1, BDD_LIST_NULL);
}
;
Elements :  Elements ',' Expr
{
  /* Like to interpret MSB in text on the left, but this is bit 0
     of the BDD vector.
  */
  $$ = bdd_list_append_cont ($3, $1);
}
;

Statement : Definition
          | Write ')'
	  ;

Write : WRITE_SYM '(' STRING
{
  /* Strange grammar rule (')' not here) because otherwise
     yytext will contain ")" and not the wanted string.
     This because we are not saving a copy of the string in lex.l
  */
  fprintf (stdout, "%s", $3);
/*  free ($3);*/
}
;

Def_Expr : LET_SYM identifier '=' Expr
{
  $$ = make_definition ($2, $4);
}
;
Def_Expr : LET_SYM identifier '=' Def_Expr
{
  $$ = make_definition ($2, $4);
}
;

Definition : Def_Expr
{
  bdd_free ($1);
}
;

identifier : IDENTIFIER
{
  $$ = make_sub_var ($1, strlen ($1));
  free ($1);
}
;

Rule : Head
;
Rule : Head IMPLIED_BY_SYM Body
{
  BDDPTR R;

  R = bdd_ite_const ($3, $1, BDD_1);
  if (BDD_VOID_P (R))
    R = BDD_0;

  $$ = bdd_assign (R);
  bdd_free ($1);
  bdd_free ($3);
}
;

Head :
{
  $$ = bdd_0 ();
}
;
Head : Expr
;
Head : Head ':' Expr
{
  $$ = bdd_or ($1, $3);
  bdd_free ($1);
  bdd_free ($3);
}
;

Body :
{
  $$ = bdd_1 ();
}
;
Body : Def_Expr
;
Body : Expr
;
Body : Body ',' Expr
{
  $$ = bdd_and ($1, $3);
  bdd_free ($1);
  bdd_free ($3);
}
;
Body : Body ',' Def_Expr
{
  $$ = bdd_and ($1, $3);
  bdd_free ($1);
  bdd_free ($3);
}
;

Expr : Identity
;

Identity : PL_formula
;
Identity : PL_formula '=' PL_formula
{
  $$ = bdd_equal_p ($1, $3) ? bdd_1 () : bdd_0 ();
  bdd_free ($1);
  bdd_free ($3);
}
;

PL_formula : PL_formula_1
;
PL_formula : PL_formula_1 '?' PL_formula ':' PL_formula
{
  $$ = bdd_ite ($1, $3, $5);
  bdd_free ($1);
  bdd_free ($3);
  bdd_free ($5);
}
;
PL_formula : PL_formula_1 '[' Body '/' variable ']'
{
  $$ = bdd_subst ($3, BDD_VARID ($5), $1);
  bdd_free ($1);
  bdd_free ($3);
  bdd_free ($5);
}
;
PL_formula : PL_formula_1 '[' variable ASSIGN Body ']'
{
  $$ = bdd_subst ($5, BDD_VARID ($3), $1);
  bdd_free ($1);
  bdd_free ($3);
  bdd_free ($5);
}
;
PL_formula : E_SYM Body '.' PL_formula
{
#if 0
  BDD_LIST vars = bdd_support_as_list_of_vars ($2);

  bdd_free ($2);
  $$ = bdd_quantify (1, $4, vars);
  bdd_free ($4);
  bdd_list_free (vars, 0);
#else
  BDDPTR vars = bdd_support_as_cube ($2);

  bdd_free ($2);
  $$ = bdd_quantify_c (1, $4, vars);
  bdd_free ($4);
  bdd_free(vars);
#endif
}
;
PL_formula : E_SYM Body '.' '(' cofactor AND_SYM cofactor ')'
{
  BDD_LIST vars;

  vars = bdd_support_as_list_of_vars ($2);
  bdd_free ($2);
  $$ = bdd_and_smooth ($5, $7, vars);
  bdd_free ($5);
  bdd_free ($7);
  bdd_list_free (vars, 0);
}
;
PL_formula : A_SYM Body '.' PL_formula
{
  BDD_LIST vars;

  vars = bdd_support_as_list_of_vars ($2);
  bdd_free ($2);
  $$ = bdd_quantify (0, $4, vars);
  bdd_free ($4);
  bdd_list_free (vars, 0);
}
;
PL_formula : D_SYM Body '.' PL_formula
{
  BDD_LIST vars;

  vars = bdd_support_as_list_of_vars ($2);
  bdd_free ($2);
  $$ = bdd_diff ($4, vars);
  bdd_free ($4);
  bdd_list_free (vars, 0);
}
;

PL_formula_1 : formula
;
PL_formula_1 : PL_formula_1 IMPLIES_SYM formula
{
  $$ = bdd_implies ($1, $3);
  bdd_free ($1);
  bdd_free ($3);
}
;
PL_formula_1 : PL_formula_1 EQUIV_SYM formula
{
  $$ = bdd_equiv ($1, $3);
  bdd_free ($1);
  bdd_free ($3);
}
;
PL_formula_1 : PL_formula_1 XOR_SYM formula
{
  $$ = bdd_xor ($1, $3);
  bdd_free ($1);
  bdd_free ($3);
}
;

formula : term
;
formula : formula OR_SYM term
{
  $$ = bdd_or ($1, $3);
  bdd_free ($1);
  bdd_free ($3);
}
;

term : cofactor
;
term : term cofactor
{
  $$ = bdd_and ($1, $2);
  bdd_free ($1);
  bdd_free ($2);
}
;
term : term AND_SYM cofactor
{
  $$ = bdd_and ($1, $3);
  bdd_free ($1);
  bdd_free ($3);
}
;

/* Right-associative. */
cofactor : factor
;
cofactor : factor '|' cofactor
{
  $$ = bdd_constrain ($1, $3);
  bdd_free ($1);
  bdd_free ($3);
}
;

factor : primary
;
factor : primary POSTFIX_NOT_SYM
{
  $$ = bdd_not ($1);
  bdd_free ($1);
}
;
factor : NOT_SYM factor
{
  $$ = bdd_not ($2);
  bdd_free ($2);
}
;

primary : atomic_formula
;
primary : '(' Body ')'
{
  $$ = $2;
}
;
primary : GC_SYM
{
  bdd_gc ();
  $$ = bdd_1 ();
}
;
primary : SUBST_SYM '(' Expr ',' variable ',' Expr ')'
{
  $$ = bdd_subst ($3, BDD_VARID ($5), $7);
  bdd_free ($3);
  bdd_free ($5);
  bdd_free ($7);
}
;
/*
primary : SUBST2_SYM '(' Expr ',' Expr ',' Expr ')'
{
  $$ = bdd_subst2 ($3, $5, $7);
  bdd_free ($3);
  bdd_free ($5);
  bdd_free ($7);
}
;
*/
primary : UNATE_IN_SYM '(' Expr ',' variable ')'
{
  $$ = bdd_unate_in ($3, BDD_VARID ($5)) ? bdd_1 () : bdd_0 ();
  bdd_free ($3);
  bdd_free ($5);
}
;
primary : ITE_SYM '(' Expr ',' Expr ',' Expr ')'
{
  $$ = bdd_ite ($3, $5, $7);
  bdd_free ($3);
  bdd_free ($5);
  bdd_free ($7);
}
;
primary : ITEC_SYM '(' Expr ',' Expr ',' Expr ')'
{
  $$ = bdd_assign (bdd_ite_const ($3, $5, $7));
  bdd_free ($3);
  bdd_free ($5);
  bdd_free ($7);
}
;
primary : TOP_SYM '(' Expr ')'
{
  $$ = bdd_top_var ($3);
  bdd_free ($3);
}
;
primary : PRIME_SYM '(' Expr ')'
{
  $$ = bdd_prime_implicant ($3);
  bdd_free ($3);
}
;
primary : MINIMIZE_X_SYM '(' Expr ')'
{
  $$ = bdd_minimize_dontcares ($3);
  bdd_free ($3);
}
;
/*
primary : MINIMIZE_SYM '(' Expr ')'
{
  $$ = bdd_subst_vars ($3);
  bdd_free ($3);
}
;
*/
primary : FREE_SYM '(' Expr ')'
{
  bdd_free ($3);
  $$ = $3;
}
;
primary : FREEZE_SYM '(' Expr ')'
{
  $$ = bdd_freeze ($3);
}
;
primary : ON_SET_SYM '(' Expr ')'
{
  $$ = bdd_on_set ($3);
  bdd_free ($3);
}
;
primary : OFF_SET_SYM '(' Expr ')'
{
  $$ = bdd_off_set ($3);
  bdd_free ($3);
}
;
primary : X_SET_SYM '(' Expr ')'
{
  $$ = bdd_dontcare_set ($3);
  bdd_free ($3);
}
;
primary : REPLACE_X_SYM '(' Expr ',' Expr ')'
{
  $$ = bdd_replace_X ($3, $5);
  bdd_free ($3);
  bdd_free ($5);
}
;
primary : EVAL_SYM '(' Expr ',' Expr ')'
{
  $$ = bdd_eval ($3, $5);
  bdd_free ($3);
  bdd_free ($5);
}
;
primary : REORDER_SYM '(' Expr ',' Expr ')'
{
  $$ = bdd_0 ();
  bdd_reorder_var (BDD_VARID ($3), BDD_VARID ($5));
  bdd_free ($3);
  bdd_free ($5);
}
;
primary : CUBE_SYM '(' Expr ')'
{
  $$ = bdd_cube_p ($3) ? bdd_1 () : bdd_0 ();
  bdd_free ($3);
}
;
primary : HAS_X_SYM '(' Expr ')'
{
  $$ = bdd_has_dontcare ($3) ? bdd_1 () : bdd_0 ();
  bdd_free ($3);
}
;
primary : IN_SYM '(' variable ',' Expr ')'
{
  $$ = bdd_in_support (BDD_VARID ($3), $5) ? bdd_1 () : bdd_0 ();
  bdd_free ($3);
  bdd_free ($5);
}
;
primary : INV_INPUT_SYM '(' Expr ',' variable ')'
{
  $$ = bdd_invert_input ($3, BDD_VARID ($5));
  bdd_free ($3);
  bdd_free ($5);
}
;
primary : SUPPORT_SYM '(' Expr ')'
{
  $$ = bdd_support_as_cube ($3);
  bdd_free ($3);
}
;
primary : INV_INPUTS_SYM '(' Expr ')'
{
  $$ = bdd_invert_inputs ($3);
  bdd_free ($3);
}
;
primary : SET_SYM '(' Expr ')'
{
  $$ = bdd_1 ();
  print_sat_assignment (stdout, $3);
  bdd_free ($3);
}
;
primary : ONE_OF_SYM '(' arg_list ')'
{
  $$ = mk_one_of ($3);
  /* mk_one_of will free the list! */
}
;
primary : PATH_SYM '(' Expr ',' Expr ')'
{
  int to_1 = !BDD_0_P ($5);
  int len;

  $$ = bdd_shortest_path_as_cube ($3, to_1, &len);
  fprintf (stdout, "/* Shortest path length is %d. */\n", len);
  bdd_free ($3);
  bdd_free ($5);
}
;
primary : MTERMS_SYM '(' Expr ',' Expr ')'
{
  char buf[BUFSIZ];

  D_sprintf (buf, bdd_count_sat_assignments ($3, $5), 0);
  fprintf (stdout, "%s minterms\n", buf);

  $$ = bdd_0 ();
  bdd_free ($3);
  bdd_free ($5);
}
;
primary : CORE_SYM '(' Expr ')'
{
  $$ = bdd_cube_factor ($3);
  bdd_free ($3);
}
;
primary : SWAP_SYM '(' Expr ')'
{
  $$ = bdd_swap_odd_even_vars ($3);
  bdd_free ($3);
}
;

variable : IDENTIFIER
{
  $$ = make_user_var ($1, strlen ($1));
  free ($1);
}
;

arg_list : Expr
{
  $$ = bdd_list_push_cont ($1, BDD_LIST_NULL);
}
;

arg_list : arg_list ',' Expr
{
  $$ = bdd_list_append_cont ($3, $1);
}
;

atomic_formula : VOID_SYM
{
  $$ = BDD_VOID;
}
;
atomic_formula : FALSE_SYM
{
  $$ = bdd_0 ();
}
;
atomic_formula : TRUE_SYM
{
  $$ = bdd_1 ();
}
;
atomic_formula : DONTCARE_SYM
{
  $$ = bdd_X ();
}
;
atomic_formula : IDENTIFIER
{
  $$ = var_access ($1, strlen ($1));
  free ($1);
}
;
/*
atomic_formula : vector
{
  BDDPTR *F = $1;

  $$ = bdd_assign (F[0]);
  FreeBDDVec (F);
}
;
*/

%%

void yyerror (char *format, ...)
{
  va_list ap;

  va_start (ap, format);

  fprintf (stderr, "%s: line %d, Error: ", filename, yylineno);
  vfprintf (stderr, format, ap);
  fprintf (stderr, " (near `%s').\n", yytext);

  va_end (ap);
}

void yywarning (char *format, ...)
{
  if (warnings) {
    va_list ap;

    va_start (ap, format);

    fprintf (stderr, "%s: line %d, Warning: ", filename, yylineno);
    vfprintf (stderr, format, ap);
    fprintf (stderr, " (near `%s').\n", yytext);

    va_end (ap);
  }
}

#ifdef COMMENT
primary : CUT_SYM '(' Expr ',' variable ')'
{
  $$ = bdd_cut ($3, BDD_VARID ($5));
  bdd_free ($3);
  bdd_free ($5);
}
;
primary : C2V_SYM '(' Expr ',' variable ')'
{
  BDDPTR F;
  BDDPTR R;

  Char2Vec ($3, &F, BDD_VARID ($5), 1);
  $$ = F;
  bdd_free ($3);
  bdd_free ($5);
}
;
primary : V2C_SYM '(' vector ',' variable ',' Expr ')'
{
  BDDPTR *F = $3;
  int size = BDDVEC_SIZE (F);

  if (verbose)
    bdd_print_vec_as_sum_of_cubes (stdout, F, size, 0);
  $$ = Vec2Char_1 (F, BDD_VARID ($5), size, $7);
  FreeBDDVec (F);
  bdd_free ($5);
  bdd_free ($7);
}
;

Var_List : IDENTIFIER
{
  BDDPTR v = make_user_var ($1, strlen ($1));

  free ($1);

  $$ = bdd_list_append_cont ((void *) BDD_VARID (v), BDD_LIST_NULL);
  bdd_free (v);
}
;
Var_List : Var_List ',' IDENTIFIER
{
  BDDPTR v = make_user_var ($3, strlen ($3));

  free ($3);

  $$ = bdd_list_append_cont ((void *) BDD_VARID (v), $1);
  bdd_free (v);
}
;
primary : TRANS_SYM '(' Expr ';' Body ';' Body ')'
{
  BDD_LIST vars_1, vars_2;

  vars_1 = bdd_support_as_list_of_vars ($5);
  bdd_free ($5);
  vars_2 = bdd_support_as_list_of_vars ($7);
  bdd_free ($7);

  $$ = trans_closure ($3, vars_1, vars_2);
  bdd_free ($3);
  bdd_list_free (vars_1, 0);
  bdd_list_free (vars_2, 0);
}
;
#endif
