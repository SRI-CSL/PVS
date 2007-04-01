%{
/*
 DOCUMENTATION INFORMATION                                 module: MU CALCULUS
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system    : Hewlett-Packard HP9000/S750
 file      : yacc.y
 unit-title: YACC GRAMMAR RULES FOR MU INPUT
 ref.      : 
 author(s) : Copyright (c) 1992-1997 G.L.J.M. Janssen
 date      :  6-FEB-1997
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/
%}

/* The Reserved Words: */
%token DOMAIN_SYM
%token LET_SYM
%token MU_SYM
%token NU_SYM
%token WRITE_SYM
%token REACH_PF
%token ONE_OF_PF
%token NONE_OF_PF

/* Identifier and Constants: */
%token IDENTIFIER
%token STRING
%token ZERO
%token <ival> POSINT

/* Operators: */
%token NOT_SYM
%token AND_SYM
%token OR_SYM
%token IMPLIES_SYM
%token IMPLIED_BY_SYM
%token EQUIV_SYM
%token XOR_SYM

/* Predefined Functions: */

/* Punctuation: */
%token ASSIGN

%token A_SYM
%token D_SYM
%token E_SYM
%token L_SYM

%{
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include "alloc.h"
#include "bdd_fns.h"
#include "mu.h"

extern char yytext[];
extern int yyleng;
extern int yylineno;

extern int warnings;
extern char *filename;

#ifdef COMMENT
static void print_int (FILE *fp, void *cont)
{
  fprintf (fp, "%d", (int) cont);
}
#endif
static void print_name (FILE *fp, void *cont)
{
  fprintf (fp, "%s", (char *) cont);
}

%}

%union
{
  long int ival;
  char *str;
  LIST list;
  Formula formula;
  Term term;
}

%type <str>	IDENTIFIER
%type <str>	STRING
%type <str>	Var
%type <ival>	R_Var_Dcl
%type <ival>	Bool_Var_Access
%type <list>	Bool_Var_List
%type <list>	Distinct_Bool_Var_List
%type <list>	Var_Dcl_List
%type <list>	Formula_List Arguments
%type <formula>	MU_Rest
%type <formula>	MU_Input
%type <formula>	Formula
%type <formula>	Quantified_Formula
%type <formula>	Formula_1
%type <formula> Formula_2
%type <formula>	Formula_3
%type <formula>	Formula_4
%type <formula>	Formula_5
%type <formula>	Atomic_Formula
%type <formula>	Primitive_Formula
%type <formula> Application
%type <term>	Term
%type <term>	Abstraction
%type <term>	Fixed_Point
%type <term>	Term_1
%type <term>	Term_2
%type <term>	Term_3
%type <term>	Term_4
%type <term>	Atomic_Term
%type <term>	Primitive_Term
%type <term>	Predefined_Term

%start MU_File

%%
MU_File : 
          {
	    if (mu_echo)
	      fprintf (stdout, "domain = { };\n");
	  }
          MU_Rest
        | Signature MU_Rest
        ;

Signature : DOMAIN_SYM '=' '{' Var_Dcl_List '}' ';'
{
  if (mu_echo)
    print_list (stdout, "domain = { ", $4, print_name, ", ", " };\n");

  mu_mk_signature ($4);
}
;

Var_Dcl_List : IDENTIFIER
               { $$ = append_cont ((void *) $1, NULL_LIST); }
             | Var_Dcl_List ',' IDENTIFIER
               { $$ = append_cont ((void *) $3, $1); }
             ;

MU_Rest : /* EMPTY */
          { $$ = NULL; }
        | MU_Input
        | MU_Input ';'
        ;

MU_Input : Formula_or_Statement
           {}
         | MU_Input ';' Formula_or_Statement
         ;

Formula_or_Statement : Formula
{
  BDDPTR R;

  if (mu_echo) {
    mu_print_formula_infix (stdout, $1);
    fprintf (stdout, ";\n");
  }
  R = mu_interpret_formula ($1, Ip, NULL);
  bdd_print_as_sum_of_cubes (stdout, R, 0);
  if (mu_verbose)
    fprintf (stdout, "Formula amounts to %d BDD nodes.\n", bdd_size (R));
  bdd_free (R);
  mu_free_formula ($1);
}
                     | Statement
                     ;

Statement : Definition
          | Write ')'
	  ;

Write : WRITE_SYM '(' STRING
{
  /* Strange grammar rule (R_PAREN not here) because otherwise
     yytext will contain ")" and not the wanted string.
     This because we are not saving a copy of the string in lex.l
  */
  fprintf (stdout, "%s", $3);
/*  free ($3);*/
}
;

Definition : LET_SYM R_Var_Dcl '=' Term
             { mu_mk_let ($2, $4); }
           ;

Formula : Formula_1
        | Formula_1 '?' Formula ':' Formula
          { $$ = mu_mk_ite_formula ($1, $3, $5); }
        | Formula_1 '[' Formula '/' Bool_Var_Access ']'
          { $$ = mu_mk_subst ($1, $5, $3); }
        | Formula_1 '[' Bool_Var_Access ASSIGN Formula ']'
          { $$ = mu_mk_subst ($1, $3, $5); }
        | Quantified_Formula
        ;

Quantified_Formula : A_SYM Bool_Var_List '.' Formula
                     { $$ = mu_mk_quantified_formula (MU_UNIV, $2, $4); }
                   | D_SYM Bool_Var_List '.' Formula
                     { $$ = mu_mk_quantified_formula (MU_DIFF, $2, $4); }
                   | E_SYM Bool_Var_List '.' Formula
                     { $$ = mu_mk_quantified_formula (MU_EXIST, $2, $4); }
                   ;

Formula_1 : Formula_2
          | Formula_1 IMPLIES_SYM Formula_2
            { $$ = mu_mk_binary_formula (MU_IMPLIES, $1, $3); }
          | Formula_1 IMPLIED_BY_SYM Formula_2
            { $$ = mu_mk_binary_formula (MU_IMPLIES, $3, $1); }
          | Formula_1 EQUIV_SYM Formula_2
            { $$ = mu_mk_binary_formula (MU_EQUIV, $1, $3); }
          | Formula_1 XOR_SYM Formula_2
            { $$ = mu_mk_binary_formula (MU_XOR, $1, $3); }
          ;

Formula_2 : Formula_3
          | Formula_2 OR_SYM Formula_3
            { $$ = mu_mk_binary_formula (MU_OR, $1, $3); }
          ;

Formula_3 : Formula_4
          | Formula_3 AND_SYM Formula_4
            { $$ = mu_mk_binary_formula (MU_AND, $1, $3); }
          ;

Formula_4 : Formula_5
          | Formula_4 '|' Formula_5
            { $$ = mu_mk_binary_formula (MU_COFACTOR, $1, $3); }
          ;

Formula_5 : Atomic_Formula
          | NOT_SYM Formula_5
            { $$ = mu_mk_unary_formula (MU_NOT, $2); }
          ;

Atomic_Formula : Primitive_Formula
               | '(' Formula ')'
                 { $$ = $2; }
               ;

Primitive_Formula : ZERO
                    { $$ = mu_mk_false_formula (); }
                  | POSINT
                    { $$ = mu_mk_true_formula (); }
                  | Var
                    {
		      mu_check_bool_var ($1);
		      $$ = mu_mk_bool_var ($1);
		    }
                  | Var '\''
                    {
		      mu_check_bool_var ($1);
		      $$ = mu_mk_unary_formula (MU_NOT, mu_mk_bool_var ($1));
		    }
                  | Application
                  ;

Application : Atomic_Term Arguments
              { $$ = mu_mk_application ($1, $2, 0/*no currying*/); }
            | ONE_OF_PF Arguments
              { $$ = mu_mk_one_of ($2); }
            | NONE_OF_PF Arguments
              { $$ = mu_mk_none_of ($2); }
            ;

/* reduce/reduce conflicts!
Arguments : Formula_5
            { $$ = append_cont ((void *) $1, NULL_LIST); }
          ;
*/
Arguments : '(' Formula_List ')'
            { $$ = $2; }
          ;

Formula_List : Formula
               { $$ = append_cont ((void *) $1, NULL_LIST); }
             | Formula_List ',' Formula
               { $$ = append_cont ((void *) $3, $1); }
             ;

Bool_Var_List : Bool_Var_Access
                { $$ = append_cont ((void *) $1, NULL_LIST); }
              | Bool_Var_List ',' Bool_Var_Access
                { $$ = append_cont ((void *) $3, $1); }
              ;

Bool_Var_Access : Var
                  {
		    /* Check Var in Domain: */
		    $$ = mu_check_bool_var ($1);
		  }
                ;

Distinct_Bool_Var_List : Bool_Var_Access
                         { $$ = append_cont ((void *) $1, NULL_LIST); }
                       | Distinct_Bool_Var_List ',' Bool_Var_Access
{
  if (in_list ((void *) $3, $1, 0 /* test == */)) {
    yyerror ("Multiple occurrence of variable `%s'; skipped",
	     mu_bool_var_name ($3));
    $$ = $1;
  }
  else
    $$ = append_cont ((void *) $3, $1);
}
;

Var : IDENTIFIER
      { $$ = $1; }
    ;

Term : Term_1
     | Abstraction
     | Fixed_Point
     ;

Abstraction : L_SYM Distinct_Bool_Var_List '.' Formula
              { $$ = mu_mk_abstraction ($2, $4); }
            ;

Fixed_Point : MU_SYM R_Var_Dcl '.' Term
              { $$ =
		  mu_mk_fixed_point (MU_L_FIXED_POINT, Ip, $2, $4, INT_MAX); }
            | MU_SYM '(' ZERO ')' R_Var_Dcl '.' Term
              { $$ = mu_mk_fixed_point (MU_L_FIXED_POINT, Ip, $5, $7, 0); }
            | MU_SYM '(' POSINT ')' R_Var_Dcl '.' Term
              { $$ = mu_mk_fixed_point (MU_L_FIXED_POINT, Ip, $5, $7, $3); }
            | NU_SYM R_Var_Dcl '.' Term
              { $$ =
		  mu_mk_fixed_point (MU_G_FIXED_POINT, Ip, $2, $4, INT_MAX); }
            | NU_SYM '(' ZERO ')' R_Var_Dcl '.' Term
              { $$ = mu_mk_fixed_point (MU_G_FIXED_POINT, Ip, $5, $7, 0); }
            | NU_SYM '(' POSINT ')' R_Var_Dcl '.' Term
              { $$ = mu_mk_fixed_point (MU_G_FIXED_POINT, Ip, $5, $7, $3); }
            ;

Term_1 : Term_2
       | Term_1 IMPLIES_SYM Term_2
         { $$ = mu_mk_binary_term (MU_T_IMPLIES, $1, $3); }
       | Term_1 IMPLIED_BY_SYM Term_2
         { $$ = mu_mk_binary_term (MU_T_IMPLIES, $3, $1); }
       | Term_1 EQUIV_SYM Term_2
         { $$ = mu_mk_binary_term (MU_T_EQUIV, $1, $3); }
       | Term_1 XOR_SYM Term_2
         { $$ = mu_mk_binary_term (MU_T_XOR, $1, $3); }
       ;

Term_2 : Term_3
       | Term_2 OR_SYM Term_3
         { $$ = mu_mk_binary_term (MU_T_OR, $1, $3); }
       ;

Term_3 : Term_4
       | Term_3 AND_SYM Term_4
         { $$ = mu_mk_binary_term (MU_T_AND, $1, $3); }
       ;

Term_4 : Atomic_Term
       | NOT_SYM Term_4
         { $$ = mu_mk_unary_term (MU_T_NOT, $2); }
       ;

Atomic_Term : Primitive_Term
            | Predefined_Term
            | '[' Term ']'
              { $$ = $2; }
            | '[' Atomic_Term Arguments ']'
              { $$ = mu_mk_curry (mu_mk_application ($2, $3, 1/*curried*/)); }
            ;

Primitive_Term : ZERO
                 { $$ = mu_mk_false_term (); }
               | POSINT
                 { $$ = mu_mk_true_term (); }
               | Var
                 { $$ = mu_mk_rel_var (Ip, $1); }
               | Var '\''
                 { $$ = mu_mk_unary_term (MU_T_NOT, mu_mk_rel_var (Ip, $1)); }
               ;

Predefined_Term : REACH_PF '(' Term ',' Term ')'
                  { $$ = mu_mk_reach ($3, $5, mu_mk_true_term ()); }
                | REACH_PF '(' Term ',' Term ',' Term ')'
                  { $$ = mu_mk_reach ($3, $5, $7); }
                ;

R_Var_Dcl : Var
            { $$ = mu_mk_rel_var_dcl ($1); }
          ;

%%

void yyerror (const char *format, ...)
{
  va_list ap;

  va_start (ap, format);

  fprintf (stderr, "[%s:%d, near `%s'] Error: ",
	   filename, yylineno, yytext);
  vfprintf (stderr, format, ap);
  fprintf (stderr, ".\n");

  va_end (ap);
}

void yywarning (const char *format, ...)
{
  if (warnings) {
    va_list ap;

    va_start (ap, format);

    fprintf (stderr, "[%s:%d, near `%s'] Warning: ",
	     filename, yylineno, yytext);
    vfprintf (stderr, format, ap);
    fprintf (stderr, ".\n");

    va_end (ap);
  }
}
