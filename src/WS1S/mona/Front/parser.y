/* 
 * parser.y
 *
 * MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
 *
 * Reproduction of all or part of this software is permitted for
 * educational or research use on condition that this copyright notice is
 * included in any copy. This software comes with no warranty of any
 * kind. In no event will BRICS be liable for any damages resulting from
 * use of this software.
 */

%{
#include <stdio.h> 
#include <string.h>
#include "untyped.h"
#include "env.h"

extern MonaUntypedAST *untypedAST;
extern Environment environment;
extern char *file;
extern bool anyUniverses;

extern void loadFile(char *);
extern void yyerror(const char *);
extern int yylex(); 

#define POS(p) Pos(p.first_line, p.first_column, file)

void check_bits(char *s)
{
  int i;
  for (i = 0; s[i] != '\0'; i++)
    if (s[i] != '0' && s[i] != '1')
      yyerror("parse error");           
}

%}

/* Values */

%union
{ 
  char *string;
  int integer;
  MonaUntypedAST *monaUntypedAST;
  DeclarationList *declarationList;
  Declaration *declaration;
  UntypedExp *untypedExp;
  ArithExp *arithExp;
  ParDeclList *parDeclList;
  BindExpList *bindExpList;
  BindExp *bindExp;
  UntypedExpList *untypedExpList;
  UntypedPragmaList *untypedPragmaList;
  UntypedPragma *untypedPragma;
  UntypedPragmaConstraintList *untypedPragmaConstraintList;
  NameList *nameList;
  Name *name;
  VarDeclList *varDeclList;
  GuideFuncList *guideFuncList;
  GuideFunc *guideFunc;
  UnivList *univList;
  Univ *univ;
  ImportMapList *mapList;
}

/* Terminals */

%token tokALL0 tokALL1 tokALL2 tokAND tokARROW tokASSERT
%token tokBIIMPL tokCOLON tokCOMMA 
%token tokCONST tokDEFAULT1 tokDEFAULT2 tokDOT tokEMPTY
%token tokEQUAL tokEX0 tokEX1 tokEX2 tokFALSE tokGREATER 
%token tokGREATEREQ tokGUIDE tokIMPL tokIN tokINTER 
%token tokINTERVAL tokLBRACE tokLBRACKET tokLESS 
%token tokLESSEQ tokLET0 tokLET1 tokLET2 tokLINEAR 
%token tokLPAREN tokMACRO tokMAX tokMIN tokMINUS
%token tokMODULO tokNOT tokNOTEQUAL tokNOTIN tokOR
%token tokPLUS tokPRED tokRBRACE
%token tokRBRACKET tokROOT tokRPAREN tokSEMICOLON 
%token tokSETMINUS tokSLASH tokSTAR
%token tokSUB tokTREE tokTRUE tokUNION tokUNIVERSE tokUP 
%token tokVAR0 tokVAR1 tokVAR2 tokWHERE tokINCLUDE
%token tokIMPORT tokEXPORT tokPREFIX tokM2LSTR tokM2LTREE tokLASTPOS
%token tokINSTATESPACE tokEXECUTE

%token <string> tokINT tokNAME tokSTRING

/* Nonterminal types */

%type <monaUntypedAST> start;
%type <declarationList> neheaders headers declarations;
%type <declaration> header declaration;
%type <untypedExp> exp where;
%type <arithExp> arith_exp;
%type <parDeclList> par_list;
%type <bindExpList> defs;
%type <bindExp> def;
%type <untypedExpList> set_body non_empty_set_body non_empty_exp_list exp_list;
%type <untypedPragmaList> pragmadecl pragma_list;
%type <untypedPragma> pragma;
%type <untypedPragmaConstraintList> constraint_list;
%type <integer> weight pragma_operator;
%type <nameList> pragma_name_list universe name_list;
%type <name> name;
%type <varDeclList> name_where_list;
%type <guideFuncList> func_list;
%type <guideFunc> func;
%type <univList> univs;
%type <univ> univ;
%type <mapList> map_list;

/* Associativity and precedence */

%nonassoc LOW
%nonassoc tokCOLON
%right tokBIIMPL
%right tokIMPL
%left tokOR
%left tokAND
%nonassoc tokNOT
%nonassoc tokIN tokNOTIN tokSUB
%nonassoc tokEQUAL tokNOTEQUAL tokGREATER tokGREATEREQ tokLESS tokLESSEQ
%nonassoc tokMAX tokMIN
%left tokUNION
%left tokINTER
%left tokSETMINUS
%left tokPLUS tokMINUS
%left tokSTAR tokSLASH tokMODULO
%nonassoc tokDOT tokUP

/* Rules */

%% 

start	: headers declarations
		{untypedAST = new MonaUntypedAST($1, $2);}
	;

headers	: /* empty */
		{$$ = NULL;}
	| neheaders
		{$$ = $1;}
	;

neheaders : header tokSEMICOLON neheaders
		{if ($1) $3->push_front($1); $$ = $3;}
	| header tokSEMICOLON
		{$$ = new DeclarationList(); 
	         if ($1) $$->push_front($1);}
	;

header	: tokLINEAR
		{if (environment.mode != DEFAULT) yyerror("parse error");
		 environment.mode = LINEAR; $$ = NULL;}
	| tokTREE
		{if (environment.mode != DEFAULT) yyerror("parse error");
		 environment.mode = TREE; $$ = NULL;}
	| tokM2LSTR
		{if (environment.mode == TREE) yyerror("parse error");
		 environment.m2l = true; environment.mode = LINEAR; $$ = NULL;}
	| tokM2LTREE
		{if (environment.mode == LINEAR) yyerror("parse error");
		 environment.m2l = true; environment.mode = TREE; $$ = NULL;}
	| tokGUIDE func_list
        	{$$ = new Guide_Declaration($2, POS(@1));}
	| tokUNIVERSE univs
        	{$$ = new Universe_Declaration($2, POS(@1));
		 anyUniverses = true;}
	;

declarations : declaration tokSEMICOLON declarations
		{if ($1) $3->push_front($1); $$ = $3;}
	| declaration tokSEMICOLON
		{$$ = new DeclarationList(); 
	         if ($1) $$->push_front($1);}
	;

declaration : tokASSERT exp  
                {$$ = new Assertion_Declaration($2, POS(@1));}
        | tokDEFAULT1 tokLPAREN name tokRPAREN tokEQUAL exp 
                {$$ = new Default_Declaration(vVar1, $3, $6, POS(@1));}
        | tokDEFAULT2 tokLPAREN name tokRPAREN tokEQUAL exp 
                {$$ = new Default_Declaration(vVar2, $3, $6, POS(@1));}
        | tokCONST name tokEQUAL arith_exp 
                {$$ = new Constant_Declaration($2, $4, POS(@1));}
        | tokVAR0 name_where_list pragmadecl 
                {$$ = new Variable_Declaration(vVar0, NULL, $2, $3, POS(@1));}
        | tokVAR1 universe name_where_list pragmadecl 
                {$$ = new Variable_Declaration(vVar1, $2, $3, $4, POS(@1));}
        | tokVAR2 universe name_where_list pragmadecl 
                {$$ = new Variable_Declaration(vVar2, $2, $3, $4, POS(@1));}
        | tokPRED name tokLPAREN par_list tokRPAREN tokEQUAL exp 
                {$$ = new Predicate_Declaration($2, $4, $7, POS(@1));}
        | tokPRED name tokEQUAL exp 
                {$$ = new Predicate_Declaration($2, new ParDeclList(), 
						$4, POS(@1));}
        | tokMACRO name tokLPAREN par_list tokRPAREN tokEQUAL exp  
                {$$ = new Macro_Declaration($2, $4, $7, POS(@1));}
        | tokMACRO name tokEQUAL exp  
                {$$ = new Macro_Declaration($2, new ParDeclList(), 
					    $4, POS(@1));}
        | exp 
                {$$ = new Expression_Declaration($1, POS(@1));}
        | tokEXECUTE exp 
                {$$ = new Execute_Declaration($2, POS(@1));}
        | tokINCLUDE tokSTRING 
                {loadFile($2+1); $$ = 0;}
	| tokLASTPOS name 
		{$$ = new LastPos_Declaration($2, POS(@1));}
        ;

exp     : name 
                {$$ = new UntypedExp_Name($1, POS(@1));}
        | tokLPAREN exp tokRPAREN
                {$$ = $2;}
        | exp tokSUB exp
                {$$ = new UntypedExp_Sub($1, $3, POS(@2));}
        | exp tokIN exp 
                {$$ = new UntypedExp_In($1, $3, POS(@2));}
        | exp tokNOTIN exp 
                {$$ = new UntypedExp_NotIn($1, $3, POS(@2));}
        | tokMIN exp
                {$$ = new UntypedExp_Min($2, POS(@1));}
        | tokMAX exp
                {$$ = new UntypedExp_Max($2, POS(@1));}
        | exp tokLESS exp
                {$$ = new UntypedExp_Less($1, $3, POS(@2));}
        | exp tokLESSEQ exp 
                {$$ = new UntypedExp_LessEq($1, $3, POS(@2));}
        | exp tokGREATEREQ exp 
                {$$ = new UntypedExp_GreaterEq($1, $3, POS(@2));}
        | exp tokGREATER exp 
                {$$ = new UntypedExp_Greater($1, $3, POS(@2));}
        | exp tokEQUAL exp
                {$$ = new UntypedExp_Equal($1, $3, POS(@2));}
        | exp tokNOTEQUAL exp 
                {$$ = new UntypedExp_NotEqual($1, $3, POS(@2));}
        | exp tokIMPL exp
                {$$ = new UntypedExp_Impl($1, $3, POS(@2));}
        | exp tokBIIMPL exp 
                {$$ = new UntypedExp_Biimpl($1, $3, POS(@2));}
        | exp tokAND exp 
                {$$ = new UntypedExp_And($1, $3, POS(@2));}
        | exp tokOR exp 
                {$$ = new UntypedExp_Or($1, $3, POS(@2));}
        | tokNOT exp 
                {$$ = new UntypedExp_Not($2, POS(@1));}
        | tokROOT tokLPAREN name tokRPAREN 
                {$$ = new UntypedExp_Root($3, POS(@1));}
        | tokROOT 
                {$$ = new UntypedExp_Root(NULL, POS(@1));}
        | exp tokDOT tokINT
                {check_bits($3);
		 $$ = new UntypedExp_Dot($1, $3, POS(@2));}
        | exp tokUP 
                {$$ = new UntypedExp_Up($1, POS(@2));}
        | tokEX0 name_where_list pragmadecl tokCOLON exp 
                {$$ = new UntypedExp_Ex0($2, $3, $5, POS(@1));}
        | tokEX1 universe name_where_list pragmadecl tokCOLON exp 
                {$$ = new UntypedExp_Ex1($2, $3, $4, $6, POS(@1));}
        | tokEX2 universe name_where_list pragmadecl tokCOLON exp 
                {$$ = new UntypedExp_Ex2($2, $3, $4, $6, POS(@1));}
        | tokALL0 name_where_list pragmadecl tokCOLON exp 
                {$$ = new UntypedExp_All0($2, $3, $5, POS(@1));}
        | tokALL1 universe name_where_list pragmadecl tokCOLON exp 
                {$$ = new UntypedExp_All1($2, $3, $4, $6, POS(@1));}
        | tokALL2 universe name_where_list pragmadecl tokCOLON exp 
                {$$ = new UntypedExp_All2($2, $3, $4, $6, POS(@1));}
        | tokLET0 defs pragmadecl tokIN exp   %prec LOW 
                {$$ = new UntypedExp_Let0($2, $3, $5, POS(@1));}
        | tokLET1 defs pragmadecl tokIN exp   %prec LOW
                {$$ = new UntypedExp_Let1($2, $3, $5, POS(@1));}
        | tokLET2 defs pragmadecl tokIN exp   %prec LOW 
                {$$ = new UntypedExp_Let2($2, $3, $5, POS(@1));}
        | name tokLPAREN exp_list tokRPAREN 
                {$$ = new UntypedExp_Call($1, $3, POS(@1));}
        | tokTRUE
                {$$ = new UntypedExp_True(POS(@1));}
        | tokFALSE 
                {$$ = new UntypedExp_False(POS(@1));}
        | tokROOT tokLPAREN exp tokCOMMA universe tokRPAREN 
                {$$ = new UntypedExp_RootPred($3, $5, POS(@1));}
        | tokEMPTY tokLPAREN exp tokRPAREN 
                {$$ = new UntypedExp_EmptyPred($3, POS(@1));}
        | exp tokPLUS arith_exp tokMODULO exp
                {$$ = new UntypedExp_PlusModulo($1, $3, $5, POS(@4));}
        | exp tokMINUS arith_exp tokMODULO exp
                {$$ = new UntypedExp_MinusModulo($1, $3, $5, POS(@4));}
        | exp tokPLUS arith_exp 
                {$$ = new UntypedExp_Plus($1, $3, POS(@2));}
        | exp tokMINUS arith_exp 
                {$$ = new UntypedExp_Minus($1, $3, POS(@2));}
        | exp tokSTAR arith_exp 
                {$$ = new UntypedExp_Mult($1, $3, POS(@2));}
        | exp tokSLASH arith_exp 
                {$$ = new UntypedExp_Div($1, $3, POS(@2));}
        | tokINT
                {$$ = new UntypedExp_Int(atoi($1), POS(@1));}
        | tokEMPTY     
                {$$ = new UntypedExp_Empty(POS(@1));}
        | tokLBRACE set_body tokRBRACE
                {$$ = new UntypedExp_Set($2, POS(@1));}
        | exp tokUNION exp               
                {$$ = new UntypedExp_Union($1, $3, POS(@2));}
        | exp tokINTER exp 
                {$$ = new UntypedExp_Inter($1, $3, POS(@2));}
        | exp tokSETMINUS exp 
                {$$ = new UntypedExp_Setminus($1, $3, POS(@2));}
	| tokIMPORT tokLPAREN tokSTRING map_list tokRPAREN 
	        {$$ = new UntypedExp_Import($3+1, $4, POS(@1));}
	| tokEXPORT tokLPAREN tokSTRING tokCOMMA exp tokRPAREN
	        {$$ = new UntypedExp_Export($3+1, $5, POS(@1));}
	| tokPREFIX tokLPAREN exp tokRPAREN
		{$$ = new UntypedExp_Prefix($3, POS(@1));}
	| tokINSTATESPACE tokLPAREN exp tokCOMMA name_list tokRPAREN
		{$$ = new UntypedExp_InStateSpace($3, $5, POS(@1));}
	;

arith_exp: arith_exp tokPLUS arith_exp 
		{$$ = new ArithExp_Add($1, $3, POS(@2));}
	| arith_exp tokMINUS arith_exp 
		{$$ = new ArithExp_Subtr($1, $3, POS(@2));}
	| arith_exp tokSTAR arith_exp 
		{$$ = new ArithExp_Mult($1, $3, POS(@2));}
	| arith_exp tokSLASH arith_exp 
                {$$ = new ArithExp_Div($1, $3, POS(@2));}
	| tokMINUS arith_exp 
		{$$ = new ArithExp_Subtr(new ArithExp_Integer(0, POS(@1)), 
					 $2, POS(@2));}    
	| tokINT
		{$$ = new ArithExp_Integer(atoi($1), POS(@1));}
	| name 
		{$$ = new ArithExp_Const($1, POS(@1));}
	| tokLPAREN arith_exp tokRPAREN 
		{$$ = $2;}
	;

par_list: tokVAR0 name tokCOMMA par_list 
		{$4->push_front(new ParDecl(pPar0, $2, NULL, POS(@2))); 
		 $$ = $4;}
	| tokVAR1 name where tokCOMMA par_list 
		{$5->push_front(new ParDecl(pPar1, $2, $3, POS(@2))); 
		 $$ = $5;}
	| tokVAR2 name where tokCOMMA par_list 
		{$5->push_front(new ParDecl(pPar2, $2, $3, POS(@2))); 
		 $$ = $5;}
	| tokUNIVERSE name tokCOMMA par_list 
		{$4->push_front(new ParDecl(pParU, $2, NULL, POS(@2))); 
		 $$ = $4;}
	| name where tokCOMMA par_list 
		{$4->push_front(new ParDecl(pPar, $1, $2, POS(@1))); 
		 $$ = $4;}
	| tokVAR0 name 
		{$$ = new ParDeclList(); 
		 $$->push_front(new ParDecl(pPar0, $2, NULL, POS(@2)));}
	| tokVAR1 name where 
		{$$ = new ParDeclList(); 
		 $$->push_front(new ParDecl(pPar1, $2, $3, POS(@2)));}
	| tokVAR2 name where
		{$$ = new ParDeclList(); 
		 $$->push_front(new ParDecl(pPar2, $2, $3, POS(@2)));}
	| tokUNIVERSE name 
		{$$ = new ParDeclList(); 
		 $$->push_front(new ParDecl(pParU, $2, NULL, POS(@2)));}
	| name where
		{$$ = new ParDeclList(); 
		 $$->push_front(new ParDecl(pPar, $1, $2, POS(@1)));}
	;

defs	: def tokCOMMA defs 
		{$3->push_front($1); 
		 $$ = $3;}
	| def 
		{$$ = new BindExpList(); 
		 $$->push_front($1);}
	;

def	: name tokEQUAL exp
		{$$ = new BindExp($1, $3, POS(@2));}     
	;

set_body: non_empty_set_body
		{$$ = $1;}
	| /* empty */
		{$$ = new UntypedExpList();}
	;

non_empty_set_body: exp tokCOMMA non_empty_set_body
                {$3->push_front($1);
                 $$ = $3;}
        | tokINTERVAL tokCOMMA non_empty_set_body
                {$3->push_front(new UntypedExp_Interval(POS(@1)));
                 $$ = $3;}
        | exp
                {$$ = new UntypedExpList();
                 $$->push_front($1);}
        | tokINTERVAL 
                {$$ = new UntypedExpList();
                 $$->push_front(new UntypedExp_Interval(POS(@1)));}
        ;

pragmadecl: tokLBRACE pragma_list tokRBRACE 
		{$$ = $2;}
	| /* empty */
		{$$ = NULL;}
	;

pragma_list: pragma 
		{$$ = new UntypedPragmaList(); 
		 $$->push_front($1);}
	| pragma tokCOMMA pragma_list 
		{$3->push_front($1); 
		 $$ = $3;}
	;

pragma	: tokINT tokCOLON pragma_name_list constraint_list 
		{$$ = new UntypedPragma(atoi($1), $3, $4);}
	| pragma_name_list constraint_list 
		{$$ = new UntypedPragma(-1, $1, $2);}
	;

constraint_list: pragma_operator weight pragma_name_list 
		{$$ = new UntypedPragmaConstraintList(); 
		 $$->push_front(new UntypedPragmaConstraint($1, $2, $3));}
	| pragma_operator weight pragma_name_list constraint_list 
		{$4->push_front(new UntypedPragmaConstraint($1, $2, $3)); 
		 $$ = $4;}
	;

weight	: tokINT
		{$$ = atoi($1);}
	| /* empty */
		{$$ = -1;}
	;

pragma_operator: tokLESS
		{$$ = 0;}
	| tokGREATER
		{$$ = 1;}
	| tokEQUAL
		{$$ = 2;}
	;

pragma_name_list: name pragma_name_list 
		{$2->push_front($1); 
		 $$ = $2;}
	| name 
		{$$ = new NameList(); 
		 $$->push_front($1);}
	;

exp_list: non_empty_exp_list 
		{$$ = $1;}
	| /* empty */
		{$$ = NULL;}
	;

non_empty_exp_list: exp tokCOMMA non_empty_exp_list 
		{$3->push_front($1); 
		 $$ = $3;}
	| exp 
		{$$ = new UntypedExpList; 
		 $$->push_front($1);}
	;

universe: tokLBRACKET name_list tokRBRACKET 
		{$$ = $2;}
	| /* empty */
		{$$ = NULL;}
	;

name	: tokNAME
                {$$ = new Name($1, POS(@1));}
	;

name_list: name tokCOMMA name_list
		{$3->push_front($1); 
		 $$ = $3;} 
	| name  
		{$$ = new NameList(); 
		 $$->push_front($1);}
	;

name_where_list: name where tokCOMMA name_where_list
		{$4->push_front(new VarDecl($1, $2, POS(@1))); 
		 $$ = $4;} 
	| name where
		{$$ = new VarDeclList(); 
		 $$->push_front(new VarDecl($1, $2, POS(@1)));}
	;

func_list: func tokCOMMA func_list
		{$3->push_front($1);
		 $$ = $3;}
	| func
		{$$ = new GuideFuncList();
		 $$->push_front($1);}
	;

func	: name tokARROW tokLPAREN name tokCOMMA name tokRPAREN
		{$$ = new GuideFunc($1, $4, $6);}
	;

univs	: univ tokCOMMA univs
		{$3->push_front($1); 
		 $$ = $3;}
	| univ
		{$$ = new UnivList(); 
		 $$->push_front($1);}
	;

univ	: name tokCOLON tokINT
		{check_bits($3);
		 $$ = new Univ($1, $3, strlen($3));}
	| name
		{$$ = new Univ($1);}
	;

where	: tokWHERE exp
		{$$ = $2;}
	| /* empty */
		{$$ = NULL;}
	;

map_list: tokCOMMA name tokARROW name map_list
		{$5->push_front(new ImportMap($2, $4, POS(@3)));
		 $$ = $5;}
	| /* empty */
		{$$ = new ImportMapList();}
	;
