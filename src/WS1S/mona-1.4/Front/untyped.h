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

#ifndef __UNTYPED_H
#define __UNTYPED_H

#include "ast.h"
#include "symboltable.h"
#include "printline.h"

extern char *file;

void TypeError(String str, Pos &p);

////////// Arithmetic expressions /////////////////////////////////////////////

enum ArithExpKind {
  aAdd, aConst, aDiv, aInteger, aMult, aSubtr
};

class ArithExp {
public:
  ArithExp(ArithExpKind k, Pos p) : 
    kind(k), pos(p) {}
  virtual ~ArithExp() {};

  virtual int evaluate() = 0;

  ArithExpKind kind;
  Pos pos;
};

class ArithExp_par_aa: public ArithExp {
public:
  ArithExp_par_aa(ArithExpKind k, ArithExp *a1, ArithExp *a2, Pos p) :
    ArithExp(k, p), aexp1(a1), aexp2(a2) {}
  virtual ~ArithExp_par_aa() {delete aexp1; delete aexp2;}

  ArithExp *aexp1;
  ArithExp *aexp2;      
};

class ArithExp_Add: public ArithExp_par_aa {
public:
  ArithExp_Add(ArithExp *aexp1, ArithExp *aexp2, Pos p) :
    ArithExp_par_aa(aAdd, aexp1, aexp2, p) {}

  int evaluate();
};

class ArithExp_Subtr: public ArithExp_par_aa {
public:
  ArithExp_Subtr(ArithExp *aexp1, ArithExp *aexp2, Pos p) :
    ArithExp_par_aa(aSubtr, aexp1, aexp2, p) {}

  int evaluate();
};

class ArithExp_Mult: public ArithExp_par_aa {
public:
  ArithExp_Mult(ArithExp *aexp1, ArithExp *aexp2, Pos p) :
    ArithExp_par_aa(aMult, aexp1, aexp2, p) {}

  int evaluate();
};

class ArithExp_Div: public ArithExp_par_aa {
public:
  ArithExp_Div(ArithExp *aexp1, ArithExp *aexp2, Pos p) :
    ArithExp_par_aa(aDiv, aexp1, aexp2, p) {}

  int evaluate();
};

class ArithExp_Integer: public ArithExp {
public:
  ArithExp_Integer(int c, Pos p) : 
    ArithExp(aInteger, p), n(c) {} 

  int evaluate();

  int n;
};

class ArithExp_Const: public ArithExp {
public:
  ArithExp_Const(Name *n, Pos p) :
    ArithExp(aConst, p), name(n) {}
  virtual ~ArithExp_Const() {delete name;}

  int evaluate();

  Name *name;
};

////////// Untyped expressions ////////////////////////////////////////////////

enum UntypedExpNodeKind {
  uAll0, uAll1, uAll2, uAnd, uBiimpl, uCall, uDiv, uDot, uEmpty, 
  uEmptyPred, uEqual, uEx0, uEx1, uEx2, uFalse, uGreater, uGreaterEq, 
  uImpl, uIn, uInt, uInter, uInterval, uLess, uLessEq, uLet0, uLet1, uLet2,
  uMax, uMin, uMinus, uMinusModulo, uMult, uName, uNot, uNotEqual, uNotIn,
  uOr, uPlus, uPlusModulo, uRoot, uSet, uSetminus, uSub, uRestrict,
  uTrue, uUnion, uUp, uImport, uExport, uPrefix, uRootPred, uInStateSpace,
  uSucc, uWellFormedTree, uType, uSomeType, uVariant, uConstTree, uTreeRoot
};

class UntypedExp {
public:
  UntypedExp(UntypedExpNodeKind k, Pos p) :
    kind(k), pos(p) {}
  virtual ~UntypedExp() {};
  
  virtual AST *genAST() = 0;

  UntypedExpNodeKind kind;
  Pos pos;
};

class UntypedExpList: public DequeGC<UntypedExp*> {};

////////// Variable declaration ///////////////////////////////////////////////

enum VarDeclKind {
  vVar0, vVar1, vVar2, vTree 
};

class VarDecl {
public:
  VarDecl() {}
  VarDecl(Name *n, UntypedExp *w, Pos p) : 
    name(n), where(w), pos(p) {}
  ~VarDecl() {delete name; delete where;}    
      
  Name *name;
  UntypedExp *where;
  Pos pos;
};

class VarDeclList: public DequeGC<VarDecl*> {};

////////// Untyped binds //////////////////////////////////////////////////////

class BindExp {
public:
  BindExp(Name *n, UntypedExp *e, Pos p) :
    name(n), exp(e), pos(p) {}
  ~BindExp() {delete name; delete exp;}

  Name *name;
  UntypedExp *exp;
  Pos pos;
};

class BindExpList: public DequeGC<BindExp*> {};

////////// Import Maps ////////////////////////////////////////////////////////

class ImportMap {
public:
  ImportMap(Name *n1, Name *n2, Pos p) :
    name1(n1), name2(n2), pos(p) {}
  ~ImportMap() {delete name1; delete name2;}

  Name *name1, *name2;
  Pos pos;
};

class ImportMapList: public DequeGC<ImportMap*> {};

////////// Types //////////////////////////////////////////////////////////////

class Component {
public:
  Component(Name *n, Name *t, Pos p) :
    name(n), type(t), pos(p) {}
  ~Component() {delete name; delete type;}

  ASTComponent *genAST();

  Name *name;
  Name *type;
  Pos pos;
};

class ComponentList: public DequeGC<Component*> {
public: 
  ASTComponentList *genAST();
};

class Variant {
public:
  Variant(Name *n, ComponentList *c, Pos p) :
    name(n), components(c), pos(p) {}
  ~Variant() {delete name; delete components;}

  ASTVariant *genAST();

  Name *name;
  ComponentList *components;
  Pos pos;
};

class VariantList: public DequeGC<Variant*> {
public:
  ASTVariantList *genAST();
};

class ConstNodeList;

class ConstNode {
public:
  ConstNode(Name *v, ConstNodeList *c, Pos p) :
    variant(v), components(c), pos(p) {}
  ~ConstNode() {delete variant; /* can't delete components here */}

  ASTForm *genAST(UntypedExp *t, Ident treevarId, Ident typeId);

  Name *variant;
  ConstNodeList *components;
  Pos pos;
};

class ConstNodeList: public Deque<ConstNode*> {
public:
  ~ConstNodeList() 
  {for (iterator i = begin(); i != end(); i++) 
    {delete (*i)->components; delete *i;}}
};

////////// Parameterized classes //////////////////////////////////////////////

class UntypedExp_par_e: public UntypedExp {
public:
  UntypedExp_par_e(UntypedExpNodeKind k, UntypedExp *e, Pos p) :
    UntypedExp(k, p), exp(e) {}
  virtual ~UntypedExp_par_e() {delete exp;}
  
  UntypedExp *exp;
};

class UntypedExp_par_ee: public UntypedExp {
public:
  UntypedExp_par_ee(UntypedExpNodeKind k, 
		    UntypedExp *e1, 
		    UntypedExp *e2, Pos p) :
    UntypedExp(k, p), exp1(e1), exp2(e2) {}
  virtual ~UntypedExp_par_ee() {delete exp1; delete exp2;}

  UntypedExp *exp1;
  UntypedExp *exp2;
};

class UntypedExp_par_npee: public UntypedExp {
public:
  UntypedExp_par_npee(UntypedExpNodeKind k, 
		      VarDeclList *d, 
		      UntypedExp *e, Pos p) :
    UntypedExp(k, p), nameList(d), exp(e) {}
  virtual ~UntypedExp_par_npee() 
  {delete nameList; delete exp;}

  VarDeclList *nameList;
  UntypedExp *exp;
};

class UntypedExp_par_unpee: public UntypedExp  {
public:
  UntypedExp_par_unpee(UntypedExpNodeKind k, 
		       NameList *ul, 
		       VarDeclList *d,
		       UntypedExp *e, Pos p) :
    UntypedExp(k, p), exp(e), univList(ul), nameList(d) {}
  virtual ~UntypedExp_par_unpee() 
  {delete nameList; delete exp; delete univList;}

  UntypedExp *exp;
  NameList *univList;
  VarDeclList *nameList;
};

class UntypedExp_par_bpe: public UntypedExp {
public:
  UntypedExp_par_bpe(UntypedExpNodeKind k, 
		     BindExpList *bl, 
		     UntypedExp *e, Pos p) :
    UntypedExp(k, p), bindList(bl), exp(e) {}
  virtual ~UntypedExp_par_bpe() 
  {delete bindList; delete exp;}
  
  BindExpList *bindList;
  UntypedExp *exp;
};

class UntypedExp_par_ea: public UntypedExp {
public:
  UntypedExp_par_ea(UntypedExpNodeKind k,  
		    UntypedExp *e, 
		    ArithExp *ae, Pos p) :
    UntypedExp(k, p), exp(e), aexp(ae) {}
  virtual ~UntypedExp_par_ea() 
  {delete exp; delete aexp;}
  
  UntypedExp *exp;
  ArithExp *aexp;
};

class UntypedExp_par_eae: public UntypedExp {
public:
  UntypedExp_par_eae(UntypedExpNodeKind k, 
		     UntypedExp *e1,
		     ArithExp *ae,
		     UntypedExp *e2, Pos p) :
    UntypedExp(k, p), exp1(e1), aexp(ae), exp2(e2) {}
  virtual ~UntypedExp_par_eae() 
  {delete exp1; delete aexp; delete exp2;}

  UntypedExp *exp1;
  ArithExp *aexp;
  UntypedExp *exp2;
};

////////// Syntactical categories of UntypedExp ///////////////////////////////

class UntypedExp_Name: public UntypedExp {
public:
  UntypedExp_Name(Name *n, Pos p) :
    UntypedExp(uName, p), name(n) {}
  virtual ~UntypedExp_Name() {delete name;}
  
  AST *genAST();

  Name *name;
};

class UntypedExp_Sub: public UntypedExp_par_ee {
public:
  UntypedExp_Sub(UntypedExp *exp1, UntypedExp *exp2, Pos p) :
    UntypedExp_par_ee(uSub, exp1, exp2, p) {}

  AST *genAST();
};

class UntypedExp_In: public UntypedExp_par_ee {
public:
  UntypedExp_In(UntypedExp *exp1, UntypedExp *exp2, Pos p) :
    UntypedExp_par_ee(uIn, exp1, exp2, p) {}

  AST *genAST();
};

class UntypedExp_NotIn: public UntypedExp_par_ee {
public:
  UntypedExp_NotIn(UntypedExp *exp1, UntypedExp *exp2, Pos p) :
    UntypedExp_par_ee(uNotIn, exp1, exp2, p) {}

  AST *genAST();        
};

class UntypedExp_Min: public UntypedExp_par_e {
public:
  UntypedExp_Min(UntypedExp *exp, Pos p) : 
    UntypedExp_par_e(uMin, exp, p) {}

  AST *genAST();
};

class UntypedExp_Max: public UntypedExp_par_e {
public:
  UntypedExp_Max(UntypedExp *exp, Pos p) :
    UntypedExp_par_e(uMax, exp, p) {}

  AST *genAST();
};

class UntypedExp_Set: public UntypedExp {
public:
  UntypedExp_Set(UntypedExpList *expList, Pos p) : 
    UntypedExp(uSet, p), expList(expList) {}
  virtual ~UntypedExp_Set() {delete expList;}

  AST *genAST();

  UntypedExpList *expList;
};


class UntypedExp_Interval: public UntypedExp {
public:
  UntypedExp_Interval(Pos p) : 
    UntypedExp(uInterval, p) {}
  
  AST *genAST();
};

class UntypedExp_Less: public UntypedExp_par_ee {
public:
  UntypedExp_Less(UntypedExp *exp1, UntypedExp *exp2, Pos p) : 
    UntypedExp_par_ee(uLess, exp1, exp2, p) {}
  
  AST *genAST();
};

class UntypedExp_LessEq: public UntypedExp_par_ee {
public:
  UntypedExp_LessEq(UntypedExp *exp1, UntypedExp *exp2, Pos p) : 
    UntypedExp_par_ee(uLessEq, exp1, exp2, p) {}

  AST *genAST(); 
};

class UntypedExp_Greater: public UntypedExp_par_ee {
public:
  UntypedExp_Greater(UntypedExp *exp1, UntypedExp *exp2, Pos p) : 
    UntypedExp_par_ee(uGreater, exp1, exp2, p) {}

  AST *genAST(); 
};

class UntypedExp_GreaterEq: public UntypedExp_par_ee {
public:
  UntypedExp_GreaterEq(UntypedExp *exp1, UntypedExp *exp2, Pos p) : 
    UntypedExp_par_ee(uGreaterEq, exp1, exp2, p) {}

  AST *genAST(); 
};

class UntypedExp_Equal: public UntypedExp_par_ee {
public:
  UntypedExp_Equal(UntypedExp *exp1, UntypedExp *exp2, Pos p) :
    UntypedExp_par_ee(uEqual, exp1, exp2, p) {}

  AST *genAST(); 
};

class UntypedExp_NotEqual: public UntypedExp_par_ee {
public:
  UntypedExp_NotEqual(UntypedExp *exp1, UntypedExp *exp2, Pos p) :
    UntypedExp_par_ee(uNotEqual, exp1, exp2, p) {}

  AST *genAST(); 
};

class UntypedExp_Union: public UntypedExp_par_ee {
public:
  UntypedExp_Union(UntypedExp *exp1, UntypedExp *exp2, Pos p) :
    UntypedExp_par_ee(uUnion, exp1, exp2, p) {}

  AST *genAST();
};

class UntypedExp_Inter: public UntypedExp_par_ee {
public:
  UntypedExp_Inter(UntypedExp *exp1, UntypedExp *exp2, Pos p) :
    UntypedExp_par_ee(uInter, exp1, exp2, p) {}

  AST *genAST();
};

class UntypedExp_Setminus: public UntypedExp_par_ee {
public:
  UntypedExp_Setminus(UntypedExp *exp1, UntypedExp *exp2, Pos p) :
    UntypedExp_par_ee(uSetminus, exp1, exp2, p) {}

  AST *genAST();
};

class UntypedExp_Impl: public UntypedExp_par_ee {
public:
  UntypedExp_Impl(UntypedExp *exp1, UntypedExp *exp2, Pos p) :
    UntypedExp_par_ee(uImpl, exp1, exp2, p) {}

  AST *genAST();
};

class UntypedExp_Biimpl: public UntypedExp_par_ee {
public:
  UntypedExp_Biimpl(UntypedExp *exp1, UntypedExp *exp2, Pos p) :
    UntypedExp_par_ee(uBiimpl, exp1, exp2, p) {}

  AST *genAST();
};

class UntypedExp_And: public UntypedExp_par_ee {
public:
  UntypedExp_And(UntypedExp *exp1, UntypedExp *exp2, Pos p) :
    UntypedExp_par_ee(uAnd, exp1, exp2, p) {}

  AST *genAST();
};

class UntypedExp_Or: public UntypedExp_par_ee {
public:
  UntypedExp_Or(UntypedExp *exp1, UntypedExp *exp2, Pos p) :
    UntypedExp_par_ee(uOr, exp1, exp2, p) {}

  AST *genAST();
};

class UntypedExp_Not: public UntypedExp_par_e {
public:
  UntypedExp_Not(UntypedExp *exp, Pos p) :
    UntypedExp_par_e(uNot, exp, p) {}

  AST *genAST();
};


class UntypedExp_Dot: public UntypedExp {
public:
  UntypedExp_Dot(UntypedExp *e, char  *bts, Pos p) :
    UntypedExp(uDot, p), bits(bts), exp(e) {}
  virtual ~UntypedExp_Dot() {delete exp;}

  AST *genAST();

  char *bits;
  UntypedExp *exp;
};

class UntypedExp_Up: public UntypedExp_par_e {
public:
  UntypedExp_Up(UntypedExp *exp, Pos p) :
    UntypedExp_par_e(uUp, exp, p) {}

  AST *genAST();
};

class UntypedExp_Ex0: public UntypedExp_par_npee  {
public:
  UntypedExp_Ex0(VarDeclList *d, 
		 UntypedExp *exp, Pos p) :
    UntypedExp_par_npee(uEx0, d, exp, p) {}  

  AST *genAST();
};

class UntypedExp_Ex1: public UntypedExp_par_unpee  {
public:
  UntypedExp_Ex1(NameList *univList, 
		 VarDeclList *d,
		 UntypedExp *exp, Pos p) :
    UntypedExp_par_unpee(uEx1, univList, d, exp, p) {}

  AST *genAST();
};

class UntypedExp_Ex2: public UntypedExp_par_unpee  {
public:
  UntypedExp_Ex2(NameList *univList, 
		 VarDeclList *d, 
		 UntypedExp *exp, Pos p) :
    UntypedExp_par_unpee(uEx2, univList, d, exp, p) {}

  AST *genAST();
};

class UntypedExp_All0: public UntypedExp_par_npee  {
public:
  UntypedExp_All0(VarDeclList *d, 
		  UntypedExp *exp, Pos p) :
    UntypedExp_par_npee(uAll0, d, exp, p) {}  

  AST *genAST();
};

class UntypedExp_All1: public UntypedExp_par_unpee  {
public:
  UntypedExp_All1(NameList *univList, 
		  VarDeclList *d, 
		  UntypedExp *exp, Pos p) :
    UntypedExp_par_unpee(uAll1, univList, d, exp, p) {}

  AST *genAST();
};

class UntypedExp_All2: public UntypedExp_par_unpee  {
public:
  UntypedExp_All2(NameList *univList, 
		  VarDeclList *d, 
		  UntypedExp *exp, Pos p) :
    UntypedExp_par_unpee(uAll2, univList, d, exp, p) {}

  AST *genAST();
};

class UntypedExp_Let0: public UntypedExp_par_bpe {
public:
  UntypedExp_Let0(BindExpList *bindList, 
		  UntypedExp *exp, Pos p) :
    UntypedExp_par_bpe(uLet0, bindList, exp, p) {}

  AST *genAST();
};

class UntypedExp_Let1: public UntypedExp_par_bpe {
public:
  UntypedExp_Let1(BindExpList *bindList, 
		  UntypedExp *exp, Pos p) :
    UntypedExp_par_bpe(uLet1, bindList, exp, p) {}

  AST *genAST();
};

class UntypedExp_Let2: public UntypedExp_par_bpe {
public:
  UntypedExp_Let2(BindExpList *bindList, 
		  UntypedExp *exp, Pos p) :
    UntypedExp_par_bpe(uLet2, bindList, exp, p) {}

  AST *genAST();
};

class UntypedExp_Call: public UntypedExp {
public:
  UntypedExp_Call(Name *n, UntypedExpList *el, Pos p) :
    UntypedExp(uCall, p), name(n), expList(el) {}
  virtual ~UntypedExp_Call() {delete name; delete expList;}

  AST *genAST();

  Name *name;
  UntypedExpList *expList;
};

class UntypedExp_Empty: public UntypedExp {
public:
  UntypedExp_Empty(Pos p) :
    UntypedExp(uEmpty, p) {}

  AST *genAST();
};

class UntypedExp_True: public UntypedExp {
public:
  UntypedExp_True(Pos p) :
    UntypedExp(uTrue, p) {}

  AST *genAST();
};

class UntypedExp_False: public UntypedExp {
public:
  UntypedExp_False(Pos p) :
    UntypedExp(uFalse, p) {}

  AST *genAST();
};

class UntypedExp_Root: public UntypedExp {
public:
  UntypedExp_Root(Name *n, Pos p) :
    UntypedExp(uRoot, p), name(n) {}
  ~UntypedExp_Root() {delete name;}

  AST *genAST();

  Name *name;
};

class UntypedExp_RootPred: public UntypedExp {
public:
  UntypedExp_RootPred(UntypedExp *e, NameList *ul, Pos p) :
    UntypedExp(uRootPred, p), exp(e), univList(ul) {}
  virtual ~UntypedExp_RootPred() {delete exp; delete univList;}

  AST *genAST();

  UntypedExp *exp;
  NameList *univList;
};

class UntypedExp_EmptyPred: public UntypedExp_par_e {
public:
  UntypedExp_EmptyPred(UntypedExp *exp, Pos p) :
    UntypedExp_par_e(uEmptyPred, exp, p) {}

  AST *genAST();
};

class UntypedExp_Plus: public UntypedExp_par_ea {
public:
  UntypedExp_Plus(UntypedExp *exp, ArithExp *aexp, Pos p) :
    UntypedExp_par_ea(uPlus, exp, aexp, p) {}
  
  AST *genAST();
};

class UntypedExp_Minus: public UntypedExp_par_ea {
public:
  UntypedExp_Minus(UntypedExp *exp, ArithExp *aexp, Pos p) :
    UntypedExp_par_ea(uMinus, exp, aexp, p) {}
  
  AST *genAST();
};

class UntypedExp_PlusModulo: public UntypedExp_par_eae {
public:
  UntypedExp_PlusModulo(UntypedExp *exp1, 
			ArithExp *aexp, 
			UntypedExp *exp2, Pos p) :
    UntypedExp_par_eae(uPlusModulo, exp1, aexp, exp2, p) {}

  AST *genAST();
};

class UntypedExp_MinusModulo: public UntypedExp_par_eae {
public:
  UntypedExp_MinusModulo(UntypedExp *exp1, 
			 ArithExp *aexp, 
			 UntypedExp *exp2, Pos p) :
    UntypedExp_par_eae(uMinusModulo, exp1, aexp, exp2, p) {}

  AST *genAST();
};


class UntypedExp_Mult: public UntypedExp_par_ea {
public:
  UntypedExp_Mult(UntypedExp *exp, ArithExp *aexp, Pos p) :
    UntypedExp_par_ea(uMult, exp, aexp, p) {} 
 
  AST *genAST();
};

class UntypedExp_Div: public UntypedExp_par_ea {
public:
  UntypedExp_Div(UntypedExp *exp, ArithExp *aexp, Pos p) :
    UntypedExp_par_ea(uDiv, exp, aexp, p) {} 

  AST *genAST();  
};

class UntypedExp_Int: public UntypedExp {
public:
  UntypedExp_Int(int c, Pos p) :
    UntypedExp(uInt, p), n(c) {}

  AST *genAST();
  
  int n;
};

class UntypedExp_Import: public UntypedExp {
public:
  UntypedExp_Import(char *f, ImportMapList *ml, Pos p) :
    UntypedExp(uImport, p), file(f), mapList(ml) {}
  virtual ~UntypedExp_Import() {delete mapList;}

  AST *genAST();
  
  char *file;
  ImportMapList *mapList;
};

class UntypedExp_Export: public UntypedExp {
public:
  UntypedExp_Export(char *f, UntypedExp *e, Pos p) :
    UntypedExp(uExport, p), file(f), exp(e) {}
  virtual ~UntypedExp_Export() {delete exp;}

  AST *genAST();
  
  char *file;
  UntypedExp *exp;
};

class UntypedExp_Prefix: public UntypedExp {
public:
  UntypedExp_Prefix(UntypedExp *e, Pos p) :
    UntypedExp(uPrefix, p), exp(e) {}
  virtual ~UntypedExp_Prefix() {delete exp;}

  AST *genAST();
  
  UntypedExp *exp;
};

class UntypedExp_InStateSpace: public UntypedExp {
public:
  UntypedExp_InStateSpace(UntypedExp *e, NameList *s, Pos p) :
    UntypedExp(uInStateSpace, p), exp(e), ss(s) {}
  virtual ~UntypedExp_InStateSpace() {delete exp; delete ss;}

  AST *genAST();
  
  UntypedExp *exp;
  NameList *ss;
};

class UntypedExp_Succ: public UntypedExp {
public:
  UntypedExp_Succ(UntypedExp *e, Name *t, Name *v, Name *c, Pos p) :
    UntypedExp(uSucc, p), exp(e), type(t), variant(v), component(c) {}
  virtual ~UntypedExp_Succ() 
  {delete exp; delete type; delete variant; delete component;}

  AST *genAST();
  
  UntypedExp *exp;
  Name *type;
  Name *variant;
  Name *component;
};

class UntypedExp_WellFormedTree: public UntypedExp {
public:
  UntypedExp_WellFormedTree(UntypedExp *e, Pos p) :
    UntypedExp(uWellFormedTree, p), exp(e) {}
  virtual ~UntypedExp_WellFormedTree() {delete exp;}

  AST *genAST();
  
  UntypedExp *exp;
};

class UntypedExp_Type: public UntypedExp {
public:
  UntypedExp_Type(UntypedExp *e, Name *t, Pos p) :
    UntypedExp(uType, p), exp(e), type(t) {}
  virtual ~UntypedExp_Type() {delete exp; delete type;}

  AST *genAST();
  
  UntypedExp *exp;
  Name *type;
};

class UntypedExp_SomeType: public UntypedExp {
public:
  UntypedExp_SomeType(UntypedExp *e, Pos p) :
    UntypedExp(uSomeType, p), exp(e) {}
  virtual ~UntypedExp_SomeType() {delete exp;}

  AST *genAST();
  
  UntypedExp *exp;
};

class UntypedExp_Variant: public UntypedExp {
public:
  UntypedExp_Variant(UntypedExp *e1, UntypedExp *e2, 
		     Name *t, Name *v, Pos p) :
    UntypedExp(uVariant, p), exp1(e1), exp2(e2), type(t), variant(v) {}
  virtual ~UntypedExp_Variant() 
  {delete exp1; delete exp2; delete type; delete variant;}

  AST *genAST();
  
  UntypedExp *exp1;
  UntypedExp *exp2;
  Name *type;
  Name *variant;
};

class UntypedExp_ConstTree: public UntypedExp {
public:
  UntypedExp_ConstTree(UntypedExp *e, Name *t, ConstNode *n, Pos p) :
    UntypedExp(uConstTree, p), exp(e), type(t), node(n) {}
  virtual ~UntypedExp_ConstTree() 
    {delete exp; delete type; delete node->components; delete node;}

  AST *genAST();
  
  UntypedExp *exp;
  Name *type;
  ConstNode *node;
};

class UntypedExp_TreeRoot: public UntypedExp {
public:
  UntypedExp_TreeRoot(UntypedExp *e, Pos p) :
    UntypedExp(uTreeRoot, p), exp(e) {}
  virtual ~UntypedExp_TreeRoot() {delete exp;}

  AST *genAST();
  
  UntypedExp *exp;
};

class UntypedExp_Restrict: public UntypedExp {
public:
  UntypedExp_Restrict(UntypedExp *e, Pos p) :
    UntypedExp(uRestrict, p), exp(e) {}
  virtual ~UntypedExp_Restrict() {delete exp;}

  AST *genAST();
  
  UntypedExp *exp;
};

////////// Guide declaration //////////////////////////////////////////////////

enum SSKind {
  SS_UNIVHAT, SS_ORHAT, SS_ORLEAF, SS_AND, SS_DUMMY, SS_NA
};

class GuideFunc {
public:
  GuideFunc(Name *n1, Name *n2, Name *n3, SSKind k = SS_NA) :
    name1(n1), name2(n2), name3(n3), kind(k) {}
  ~GuideFunc() {delete name1; delete name2; delete name3;}

  Name *name1;
  Name *name2; 
  Name *name3;
  SSKind kind;
};

class GuideFuncList: public DequeGC<GuideFunc*> {};

////////// Universe declaration ///////////////////////////////////////////////

class Univ {
public:
  Univ(Name *n, char *p = NULL, int d = 0) :
    name(n), pos(p), type(0), depth(d) {}
  Univ(Name *n, Name *t) :
    name(n), pos(0), type(t) {}
  ~Univ() {delete name; delete type;}

  Name *name;
  char *pos;
  Name *type; /* if pos==0, type is used */
  int depth;
};

class UnivList: public DequeGC<Univ*> {};

////////// Parameter declaration auxiliary classes ////////////////////////////

enum ParDeclKind {
  pPar0, pPar1, pPar2, pParU, pPar 
};

class ParDecl {
public:
  ParDecl() {}
  ParDecl(ParDeclKind k, Name *n, UntypedExp *w, Pos p) :
    kind(k), name(n), where(w), pos(p) {}
  ~ParDecl() {delete name; delete where;}

  ParDeclKind kind;
  Name *name;
  UntypedExp *where;
  Pos pos;
};

class ParDeclList: public DequeGC<ParDecl*> {};

////////// Declarations ///////////////////////////////////////////////////////

enum DeclarationKind {
  dAssertion,
  dConstant,
  dDefault,
  dExpression, 
  dGuide,
  dMacro, 
  dPredicate, 
  dUniverse, 
  dVariable, 
  dLastPos,
  dAllPos,
  dExecute,
  dType,
  dVerify
};

class Declaration {
public:
  Declaration() {}
  Declaration(DeclarationKind k, Pos p) :
    kind(k), pos(p) {}
  virtual ~Declaration() {};

  virtual void genAST(MonaAST &monaAST) = 0;
		       
  DeclarationKind kind;
  Pos pos;
};

class DeclarationList: public DequeGC<Declaration*> {};

class Predicate_Macro_Declaration: public Declaration {
public:
  Predicate_Macro_Declaration(DeclarationKind k, Name *n, ParDeclList *pars, 
			      UntypedExp *b, Pos p) :
    Declaration(k, p), name(n), parameters(pars), body(b) 
    {source = file;}
  virtual ~Predicate_Macro_Declaration() 
  {delete name; delete parameters; delete body;}

  void genAST(MonaAST &monaAST);
  
  Name *name;
  ParDeclList *parameters;
  UntypedExp *body;
  char *source;
};

class Predicate_Declaration: public Predicate_Macro_Declaration {
public:
  Predicate_Declaration(Name *n, ParDeclList *pars, UntypedExp *b, Pos p) :
    Predicate_Macro_Declaration(dPredicate, n, pars, b, p) {}
};

class Macro_Declaration: public Predicate_Macro_Declaration {
public:
  Macro_Declaration(Name *n, ParDeclList *pars, UntypedExp *b, Pos p) :
    Predicate_Macro_Declaration(dMacro, n, pars, b, p) {}
};

class Variable_Declaration: public Declaration {
public:
  Variable_Declaration(VarDeclKind k, NameList *u, 
		       VarDeclList *d, Pos p) :
    Declaration(dVariable, p), declKind(k), univs(u), decls(d) {}
  virtual ~Variable_Declaration() 
  {delete univs; delete decls;}

  void genAST(MonaAST &monaAST);
  
  VarDeclKind declKind;
  NameList *univs;
  VarDeclList *decls;
};

class Universe_Declaration: public Declaration {
public:
  Universe_Declaration(UnivList *ul, Pos p) :
    Declaration(dUniverse, p), univList(ul) {}
  virtual ~Universe_Declaration() {delete univList;}
 
  void genAST(MonaAST &monaAST);

  UnivList *univList;
};

class Expression_Declaration: public Declaration {
public:
  Expression_Declaration(UntypedExp *e, Pos p) :
    Declaration(dExpression, p), exp(e) {}
  virtual ~Expression_Declaration() {delete exp;}

  void genAST(MonaAST &monaAST);
  
  UntypedExp *exp;
};

class Verify_Declaration: public Declaration {
public:
  Verify_Declaration(char *t, UntypedExp *e, Pos p) :
    Declaration(dVerify, p), title(t), exp(e) {}
  virtual ~Verify_Declaration() {delete exp;}

  void genAST(MonaAST &monaAST);
  
  char *title;
  UntypedExp *exp;
};

class Execute_Declaration: public Declaration {
public:
  Execute_Declaration(UntypedExp *e, Pos p) :
    Declaration(dExecute, p), exp(e) {}
  virtual ~Execute_Declaration() {delete exp;}

  void genAST(MonaAST &monaAST);
  
  UntypedExp *exp;
};

class Assertion_Declaration: public Declaration {
public:
  Assertion_Declaration(UntypedExp *e, Pos p) :
    Declaration(dAssertion, p), exp(e) {}
  virtual ~Assertion_Declaration() {delete exp;}
 
  void genAST(MonaAST &monaAST);
  
  UntypedExp *exp;
};

class Constant_Declaration: public Declaration {
public:
  Constant_Declaration(Name *n, ArithExp *a, Pos p) :
    Declaration(dConstant, p), name(n), aexp(a) {}
  virtual ~Constant_Declaration() {delete name; delete aexp;}

  void genAST(MonaAST &monaAST);
  
  Name *name;
  ArithExp *aexp;
};

class Guide_Declaration: public Declaration {
public:
  Guide_Declaration(GuideFuncList *g, Pos p) :
    Declaration(dGuide, p), funcList(g) {}
  virtual ~Guide_Declaration() {delete funcList;}

  void genAST(MonaAST &monaAST);
  
  GuideFuncList *funcList;
};

class Default_Declaration: public Declaration {
public:
  Default_Declaration(VarDeclKind k, Name *n, UntypedExp *e, Pos p) :
    Declaration(dDefault, p), type(k), name(n), exp(e) {}
  virtual ~Default_Declaration() {delete name; delete exp;}

  void genAST(MonaAST &monaAST);

  VarDeclKind type;
  Name *name;
  UntypedExp *exp;
};

class LastPos_Declaration: public Declaration {
public:
  LastPos_Declaration(Name *n, Pos p) :
    Declaration(dLastPos, p), name(n) {}
  virtual ~LastPos_Declaration() {delete name;}

  void genAST(MonaAST &monaAST);
  
  Name *name;
};

class AllPos_Declaration: public Declaration {
public:
  AllPos_Declaration(Name *n, Pos p) :
    Declaration(dAllPos, p), name(n) {}
  virtual ~AllPos_Declaration() {delete name;}

  void genAST(MonaAST &monaAST);
  
  Name *name;
};

class Type_Declaration: public Declaration {
public:
  Type_Declaration(Name *n, VariantList *vl, Pos p) :
    Declaration(dType, p), name(n), variants(vl) {}
  virtual ~Type_Declaration() {delete name; delete variants;}

  void genAST(MonaAST &monaAST);
  void checkReachable();

  Name *name;
  VariantList *variants;
};

////////// Main untyped AST ///////////////////////////////////////////////////

class MonaUntypedAST {
public:
  MonaUntypedAST(DeclarationList *decls) :
    declarations(decls) {}
  ~MonaUntypedAST() {delete declarations;}

  MonaAST *typeCheck();

  // GTA guide construction (makeguide.cpp)
  void makeGTAGuide();
  char *typedUnivs2guide(unsigned num, IdentList *univs, unsigned idx, 
			 char *pos);
  char *variants2guide(unsigned num, ASTVariantList *variants,
		       unsigned idx, IdentList *typeSet, Ident typeId,
		       char *univ, char *type, char *pos);
  char* components2guide(unsigned num, ASTComponentList *components,
			 unsigned idx, IdentList *typeSet,
			 Ident typeId, unsigned variantidx,
			 char *univ, char *type, 
			 char *variant, char *pos);
  GuideFunc *makeStateSpace(char *ssname, char *pos,
			    char **leftpos, char **rightpos, SSKind kind);
  char *makeDummySS(char *univ);

  DeclarationList *declarations;
};

#endif
