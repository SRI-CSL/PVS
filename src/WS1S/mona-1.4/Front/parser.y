/*
 * MONA
 * Copyright (C) 1997-2000 BRICS.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the  Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
 * USA.
 */

%{
#include <stdio.h> 
#include <string.h>
#include "untyped.h"
#include "env.h"

extern MonaUntypedAST *untypedAST;
extern Options options;
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
  NameList *nameList;
  Name *name;
  VarDeclList *varDeclList;
  GuideFuncList *guideFuncList;
  GuideFunc *guideFunc;
  UnivList *univList;
  Univ *univ;
  ImportMapList *mapList;
  VariantList *variantList;
  ComponentList *componentList;
  ConstNode *constNode;
  ConstNodeList *constNodeList;
}

/* Terminals */

%token tokALL0 tokALL1 tokALL2 tokAND tokARROW tokASSERT
%token tokBIIMPL tokCOLON tokCOMMA 
%token tokCONST tokDEFAULT1 tokDEFAULT2 tokDOT tokEMPTY
%token tokEQUAL tokEX0 tokEX1 tokEX2 tokFALSE tokGREATER 
%token tokGREATEREQ tokGUIDE tokIMPL tokIN tokINTER 
%token tokINTERVAL tokLBRACE tokLBRACKET tokLESS 
%token tokLESSEQ tokLET0 tokLET1 tokLET2
%token tokLPAREN tokMACRO tokMAX tokMIN tokMINUS
%token tokMODULO tokNOT tokNOTEQUAL tokNOTIN tokOR
%token tokPLUS tokPRED tokRBRACE tokRESTRICT
%token tokRBRACKET tokUNIVROOT tokRPAREN tokSEMICOLON 
%token tokSETMINUS tokSLASH tokSTAR tokVERIFY
%token tokSUB tokTREE tokTRUE tokUNION tokUNIVERSE tokUP 
%token tokVAR0 tokVAR1 tokVAR2 tokWHERE tokINCLUDE
%token tokIMPORT tokEXPORT tokPREFIX tokM2LSTR tokM2LTREE tokLASTPOS
%token tokINSTATESPACE tokEXECUTE tokTYPE tokSOMETYPE tokVARIANT tokSUCC
%token tokWS1S tokWS2S tokTREEROOT tokCONSTTREE tokALLPOS

%token <string> tokINT tokNAME tokSTRING

/* Nonterminal types */

%type <monaUntypedAST> start;
%type <declarationList> declarations;
%type <declaration> declaration;
%type <untypedExp> exp where;
%type <arithExp> arith_exp;
%type <parDeclList> par_list;
%type <bindExpList> defs;
%type <bindExp> def;
%type <untypedExpList> set_body non_empty_set_body non_empty_exp_list exp_list;
%type <nameList> universe name_list;
%type <name> name;
%type <varDeclList> name_where_list;
%type <guideFuncList> func_list;
%type <guideFunc> func;
%type <univList> univs;
%type <univ> univ;
%type <mapList> map_list;
%type <variantList> variant_list;
%type <componentList> component_list;
%type <constNode> constnode;
%type <constNodeList> constnode_list;
%type <string> optstring;

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

start	: header declarations
		{untypedAST = new MonaUntypedAST($2);}
	;

header	:  tokWS1S tokSEMICOLON
		{options.m2l = false; options.mode = LINEAR;}
	| tokWS2S tokSEMICOLON
		{options.m2l = false; options.mode = TREE;}
	| tokM2LSTR tokSEMICOLON 
		{options.m2l = true; options.mode = LINEAR;}
	| tokM2LTREE tokSEMICOLON
		{options.m2l = true; options.mode = TREE;}
	| /* empty */
		{options.m2l = false; options.mode = LINEAR;}
	;

declarations : declaration declarations
		{if ($1) $2->push_front($1); $$ = $2;}
	| declaration
		{$$ = new DeclarationList(); 
	         if ($1) $$->push_front($1);}
	;

declaration : tokASSERT exp tokSEMICOLON
                {$$ = new Assertion_Declaration($2, POS(@1));}
	| tokGUIDE func_list tokSEMICOLON
        	{$$ = new Guide_Declaration($2, POS(@1));}
	| tokUNIVERSE univs tokSEMICOLON
        	{$$ = new Universe_Declaration($2, POS(@1));
		 anyUniverses = true;}
        | tokDEFAULT1 tokLPAREN name tokRPAREN tokEQUAL exp tokSEMICOLON 
                {$$ = new Default_Declaration(vVar1, $3, $6, POS(@1));}
        | tokDEFAULT2 tokLPAREN name tokRPAREN tokEQUAL exp tokSEMICOLON 
                {$$ = new Default_Declaration(vVar2, $3, $6, POS(@1));}
        | tokCONST name tokEQUAL arith_exp tokSEMICOLON 
                {$$ = new Constant_Declaration($2, $4, POS(@1));}
        | tokVAR0 name_where_list tokSEMICOLON
                {$$ = new Variable_Declaration(vVar0, NULL, $2, POS(@1));}
        | tokVAR1 universe name_where_list tokSEMICOLON
                {$$ = new Variable_Declaration(vVar1, $2, $3, POS(@1));}
        | tokVAR2 universe name_where_list tokSEMICOLON
                {$$ = new Variable_Declaration(vVar2, $2, $3, POS(@1));}
	| tokTREE universe name_where_list tokSEMICOLON
		{$$ = new Variable_Declaration(vTree, $2, $3, POS(@1));}
        | tokPRED name tokLPAREN par_list tokRPAREN tokEQUAL exp tokSEMICOLON 
                {$$ = new Predicate_Declaration($2, $4, $7, POS(@1));}
        | tokPRED name tokEQUAL exp tokSEMICOLON
                {$$ = new Predicate_Declaration($2, new ParDeclList(), 
						$4, POS(@1));}
        | tokPRED name tokLPAREN tokRPAREN tokEQUAL exp tokSEMICOLON 
                {$$ = new Predicate_Declaration($2, new ParDeclList(), 
						$6, POS(@1));}
        | tokMACRO name tokLPAREN par_list tokRPAREN tokEQUAL exp tokSEMICOLON  
                {$$ = new Macro_Declaration($2, $4, $7, POS(@1));}
        | tokMACRO name tokEQUAL exp tokSEMICOLON  
                {$$ = new Macro_Declaration($2, new ParDeclList(), 
					    $4, POS(@1));}
        | tokMACRO name tokLPAREN tokRPAREN tokEQUAL exp tokSEMICOLON  
                {$$ = new Macro_Declaration($2, new ParDeclList(), 
					    $6, POS(@1));}
        | exp tokSEMICOLON 
                {$$ = new Expression_Declaration($1, POS(@1));}
        | tokVERIFY optstring exp tokSEMICOLON 
                {$$ = new Verify_Declaration($2, $3, POS(@1));}
        | tokEXECUTE exp tokSEMICOLON 
                {$$ = new Execute_Declaration($2, POS(@1));}
        | tokINCLUDE tokSTRING tokSEMICOLON 
                {loadFile($2+1); $$ = 0;}
	| tokLASTPOS name tokSEMICOLON 
		{$$ = new LastPos_Declaration($2, POS(@1));}
	| tokALLPOS name tokSEMICOLON 
		{$$ = new AllPos_Declaration($2, POS(@1));}
	| tokTYPE name tokEQUAL variant_list tokSEMICOLON
		{$$ = new Type_Declaration($2, $4, POS(@1));}
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
        | tokUNIVROOT tokLPAREN name tokRPAREN 
                {$$ = new UntypedExp_Root($3, POS(@1));}
        | tokUNIVROOT 
                {$$ = new UntypedExp_Root(NULL, POS(@1));}
        | exp tokDOT tokINT
                {check_bits($3);
		 $$ = new UntypedExp_Dot($1, $3, POS(@2));}
        | exp tokUP 
                {$$ = new UntypedExp_Up($1, POS(@2));}
        | tokEX0 name_where_list tokCOLON exp 
                {$$ = new UntypedExp_Ex0($2, $4, POS(@1));}
        | tokEX1 universe name_where_list tokCOLON exp 
                {$$ = new UntypedExp_Ex1($2, $3, $5, POS(@1));}
        | tokEX2 universe name_where_list tokCOLON exp 
                {$$ = new UntypedExp_Ex2($2, $3, $5, POS(@1));}
        | tokALL0 name_where_list tokCOLON exp 
                {$$ = new UntypedExp_All0($2, $4, POS(@1));}
        | tokALL1 universe name_where_list tokCOLON exp 
                {$$ = new UntypedExp_All1($2, $3, $5, POS(@1));}
        | tokALL2 universe name_where_list tokCOLON exp 
                {$$ = new UntypedExp_All2($2, $3, $5, POS(@1));}
        | tokLET0 defs tokIN exp   %prec LOW 
                {$$ = new UntypedExp_Let0($2, $4, POS(@1));}
        | tokLET1 defs tokIN exp   %prec LOW
                {$$ = new UntypedExp_Let1($2, $4, POS(@1));}
        | tokLET2 defs tokIN exp   %prec LOW 
                {$$ = new UntypedExp_Let2($2, $4, POS(@1));}
        | name tokLPAREN exp_list tokRPAREN 
                {$$ = new UntypedExp_Call($1, $3, POS(@1));}
        | tokTRUE
                {$$ = new UntypedExp_True(POS(@1));}
        | tokFALSE 
                {$$ = new UntypedExp_False(POS(@1));}
        | tokUNIVROOT tokLPAREN exp tokCOMMA universe tokRPAREN 
                {if (!$5) yyerror("parse error");
		 $$ = new UntypedExp_RootPred($3, $5, POS(@1));}
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
	| tokVARIANT tokLPAREN exp tokCOMMA exp tokCOMMA name 
	  tokCOMMA name tokRPAREN
		{$$ = new UntypedExp_Variant($3, $5, $7, $9, POS(@1));}
	| tokSUCC tokLPAREN exp tokCOMMA name tokCOMMA name tokCOMMA 
	  name tokRPAREN
		{$$ = new UntypedExp_Succ($3, $5, $7, $9, POS(@1));}
	| tokTREE tokLPAREN exp tokRPAREN
		{$$ = new UntypedExp_WellFormedTree($3, POS(@1));}
	| tokTYPE tokLPAREN exp tokCOMMA name tokRPAREN
		{$$ = new UntypedExp_Type($3, $5, POS(@1));}
	| tokSOMETYPE tokLPAREN exp tokRPAREN
		{$$ = new UntypedExp_SomeType($3, POS(@1));}
	| tokCONSTTREE tokLPAREN exp tokCOMMA name tokCOLON
          constnode tokRPAREN
		{$$ = new UntypedExp_ConstTree($3, $5, $7, POS(@1));}
	| tokTREEROOT tokLPAREN exp tokRPAREN
		{$$ = new UntypedExp_TreeRoot($3, POS(@1));}     
        | tokRESTRICT tokLPAREN exp tokRPAREN
                {$$ = new UntypedExp_Restrict($3, POS(@1));}
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
	| name tokCOLON name
		{$$ = new Univ($1, $3);}
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

variant_list: name tokLPAREN component_list tokRPAREN tokCOMMA variant_list
		{$6->push_front(new Variant($1, $3, POS(@1))); 
		 $$ = $6;}
	| name tokLPAREN tokRPAREN tokCOMMA variant_list
		{$5->push_front(new Variant($1, NULL, POS(@1))); 
		 $$ = $5;}
	| name tokCOMMA variant_list
		{$3->push_front(new Variant($1, NULL, POS(@1))); 
		 $$ = $3;}
	| name tokLPAREN component_list tokRPAREN
		{$$ = new VariantList();
		 $$->push_front(new Variant($1, $3, POS(@1)));}
	| name tokLPAREN tokRPAREN
		{$$ = new VariantList();
		 $$->push_front(new Variant($1, NULL, POS(@1)));}
	| name
		{$$ = new VariantList();
		 $$->push_front(new Variant($1, NULL, POS(@1)));}
	;

component_list: name tokCOLON name tokCOMMA component_list
		{$5->push_front(new Component($1, $3, POS(@1)));
		 $$ = $5;}
	| name tokCOLON name
		{$$ = new ComponentList();
		 $$->push_front(new Component($1, $3, POS(@1)));}
	;

constnode: name tokLPAREN constnode_list tokRPAREN
		{$$ = new ConstNode($1, $3, POS(@1));}
	| name tokLPAREN tokRPAREN
		{$$ = new ConstNode($1, NULL, POS(@1));}
	| name
		{$$ = new ConstNode($1, NULL, POS(@1));}
	;

constnode_list: constnode tokCOMMA constnode_list
		{$3->push_front($1); $$ = $3;}
	| constnode
		{$$ = new ConstNodeList;
		 $$->push_front($1);}
	;

optstring: tokSTRING
           {$$ = $1+1;}
         | /* empty */
           {$$ = 0;}
         ;
