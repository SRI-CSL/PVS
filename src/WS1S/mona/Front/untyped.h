//
// untyped.h
//
// MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
// 
// Reproduction of all or part of this software is permitted for
// educational or research use on condition that this copyright notice is
// included in any copy. This software comes with no warranty of any
// kind. In no event will BRICS be liable for any damages resulting from
// use of this software.

#ifndef __UNTYPED_H
#define __UNTYPED_H

#include "ast.h"
#include "symboltable.h"
#include "printline.h"

extern char *file;

////////// Untyped pragmas ////////////////////////////////////////////////////

class UntypedPragmaConstraint {
public:
  UntypedPragmaConstraint() {}
  UntypedPragmaConstraint(int opp, int w, NameList *vl) : 
    op(opp), weight(w), varList(vl) {}
  ~UntypedPragmaConstraint() {delete varList;}

  int op;
  int weight;
  NameList *varList;
};

class UntypedPragmaConstraintList: public DequeGC<UntypedPragmaConstraint*> {};

class UntypedPragma {
public:
  UntypedPragma() {}
  UntypedPragma(int d, NameList *vl, UntypedPragmaConstraintList *items) :
    defaultWeight(d), varList(vl), pragmaItems(items) {}
  ~UntypedPragma() {delete varList; delete pragmaItems;}

  int defaultWeight;
  NameList *varList;
  UntypedPragmaConstraintList *pragmaItems;
};

class UntypedPragmaList: public DequeGC<UntypedPragma*> {};

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

class ArithExp_par_aa:public ArithExp {
public:
  ArithExp_par_aa(ArithExpKind k, ArithExp *a1, ArithExp *a2, Pos p) :
    ArithExp(k, p), aexp1(a1), aexp2(a2) {}
  virtual ~ArithExp_par_aa() {delete aexp1; delete aexp2;}

  ArithExp *aexp1;
  ArithExp *aexp2;      
};

class ArithExp_Add:public ArithExp_par_aa {
public:
  ArithExp_Add(ArithExp *aexp1, ArithExp *aexp2, Pos p) :
    ArithExp_par_aa(aAdd, aexp1, aexp2, p) {}

  int evaluate();
};

class ArithExp_Subtr:public ArithExp_par_aa {
public:
  ArithExp_Subtr(ArithExp *aexp1, ArithExp *aexp2, Pos p) :
    ArithExp_par_aa(aSubtr, aexp1, aexp2, p) {}

  int evaluate();
};

class ArithExp_Mult:public ArithExp_par_aa {
public:
  ArithExp_Mult(ArithExp *aexp1, ArithExp *aexp2, Pos p) :
    ArithExp_par_aa(aMult, aexp1, aexp2, p) {}

  int evaluate();
};

class ArithExp_Div:public ArithExp_par_aa {
public:
  ArithExp_Div(ArithExp *aexp1, ArithExp *aexp2, Pos p) :
    ArithExp_par_aa(aDiv, aexp1, aexp2, p) {}

  int evaluate();
};

class ArithExp_Integer:public ArithExp {
public:
  ArithExp_Integer(int c, Pos p) : 
    ArithExp(aInteger, p), n(c) {} 

  int evaluate();

  int n;
};

class ArithExp_Const:public ArithExp {
public:
  ArithExp_Const(Name *n, Pos p) :
    ArithExp(aConst, p), name(n) {}
  virtual ~ArithExp_Const() {delete name;}

  int evaluate();

  Name *name;
};

////////// Untyped mona expressions ///////////////////////////////////////////

enum UntypedExpNodeKind {
  uAll0, uAll1, uAll2, uAnd, uBiimpl, uCall, uDiv, uDot, uEmpty, 
  uEmptyPred, uEqual, uEx0, uEx1, uEx2, uFalse, uGreater, uGreaterEq, 
  uImpl, uIn, uInt, uInter, uInterval, uLess, uLessEq, uLet0, uLet1, uLet2,
  uMax, uMin, uMinus, uMinusModulo, uMult, uName, uNot, uNotEqual, uNotIn,
  uOr, uPlus, uPlusModulo, uRoot, uSet, uSetminus, uSub, 
  uTrue, uUnion, uUp, uImport, uExport, uPrefix, uRootPred, uInStateSpace
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
  vVar0, vVar1, vVar2 
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

////////// Parameterized classes //////////////////////////////////////////////

class UntypedExp_par_e:public UntypedExp {
public:
  UntypedExp_par_e(UntypedExpNodeKind k, UntypedExp *e, Pos p) :
    UntypedExp(k, p), exp(e) {}
  virtual ~UntypedExp_par_e() {delete exp;}
  
  UntypedExp *exp;
};

class UntypedExp_par_ee:public UntypedExp {
public:
  UntypedExp_par_ee(UntypedExpNodeKind k, 
		    UntypedExp *e1, 
		    UntypedExp *e2, Pos p) :
    UntypedExp(k, p), exp1(e1), exp2(e2) {}
  virtual ~UntypedExp_par_ee() {delete exp1; delete exp2;}

  UntypedExp *exp1;
  UntypedExp *exp2;
};

class UntypedExp_par_npee:public UntypedExp {
public:
  UntypedExp_par_npee(UntypedExpNodeKind k, 
		      VarDeclList *d, 
		      UntypedPragmaList *pgs, 
		      UntypedExp *e, Pos p) :
    UntypedExp(k, p), nameList(d), prags(pgs), exp(e) {}
  virtual ~UntypedExp_par_npee() 
  {delete nameList; delete prags; delete exp;}

  VarDeclList *nameList;
  UntypedPragmaList *prags;
  UntypedExp *exp;
};

class UntypedExp_par_unpee:public UntypedExp  {
public:
  UntypedExp_par_unpee(UntypedExpNodeKind k, 
		       NameList *ul, 
		       VarDeclList *d,
		       UntypedPragmaList *pgs, 
		       UntypedExp *e, Pos p) :
    UntypedExp(k, p), prags(pgs), exp(e), univList(ul), nameList(d) {}
  virtual ~UntypedExp_par_unpee() 
  {delete nameList; delete prags; delete exp; delete univList;}

  UntypedPragmaList *prags;
  UntypedExp *exp;
  NameList *univList;
  VarDeclList *nameList;
};

class UntypedExp_par_bpe:public UntypedExp {
public:
  UntypedExp_par_bpe(UntypedExpNodeKind k, 
		     BindExpList *bl, 
		     UntypedPragmaList *pgs,
		     UntypedExp *e, Pos p) :
    UntypedExp(k, p), bindList(bl), prags(pgs), exp(e) {}
  virtual ~UntypedExp_par_bpe() 
  {delete bindList; delete prags; delete exp;}
  
  BindExpList *bindList;
  UntypedPragmaList *prags;
  UntypedExp *exp;
};

class UntypedExp_par_ea:public UntypedExp {
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

class UntypedExp_par_eae:public UntypedExp {
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

class UntypedExp_Name:public UntypedExp {
public:
  UntypedExp_Name(Name *n, Pos p) :
    UntypedExp(uName, p), name(n) {}
  virtual ~UntypedExp_Name() {delete name;}
  
  AST *genAST();

  Name *name;
};

class UntypedExp_Sub:public UntypedExp_par_ee {
public:
  UntypedExp_Sub(UntypedExp *exp1, UntypedExp *exp2, Pos p) :
    UntypedExp_par_ee(uSub, exp1, exp2, p) {}

  AST *genAST();
};

class UntypedExp_In:public UntypedExp_par_ee {
public:
  UntypedExp_In(UntypedExp *exp1, UntypedExp *exp2, Pos p) :
    UntypedExp_par_ee(uIn, exp1, exp2, p) {}

  AST *genAST();
};

class UntypedExp_NotIn:public UntypedExp_par_ee {
public:
  UntypedExp_NotIn(UntypedExp *exp1, UntypedExp *exp2, Pos p) :
    UntypedExp_par_ee(uNotIn, exp1, exp2, p) {}

  AST *genAST();        
};

class UntypedExp_Min:public UntypedExp_par_e {
public:
  UntypedExp_Min(UntypedExp *exp, Pos p) : 
    UntypedExp_par_e(uMin, exp, p) {}

  AST *genAST();
};

class UntypedExp_Max:public UntypedExp_par_e {
public:
  UntypedExp_Max(UntypedExp *exp, Pos p) :
    UntypedExp_par_e(uMax, exp, p) {}

  AST *genAST();
};

class UntypedExp_Set:public UntypedExp {
public:
  UntypedExp_Set(UntypedExpList *expList, Pos p) : 
    UntypedExp(uSet, p), expList(expList) {}
  virtual ~UntypedExp_Set() {delete expList;}

  AST *genAST();

  UntypedExpList *expList;
};


class UntypedExp_Interval:public UntypedExp {
public:
  UntypedExp_Interval(Pos p) : 
    UntypedExp(uInterval, p) {}
  
  AST *genAST();
};

class UntypedExp_Less:public UntypedExp_par_ee {
public:
  UntypedExp_Less(UntypedExp *exp1, UntypedExp *exp2, Pos p) : 
    UntypedExp_par_ee(uLess, exp1, exp2, p) {}
  
  AST *genAST();
};

class UntypedExp_LessEq:public UntypedExp_par_ee {
public:
  UntypedExp_LessEq(UntypedExp *exp1, UntypedExp *exp2, Pos p) : 
    UntypedExp_par_ee(uLessEq, exp1, exp2, p) {}

  AST *genAST(); 
};

class UntypedExp_Greater:public UntypedExp_par_ee {
public:
  UntypedExp_Greater(UntypedExp *exp1, UntypedExp *exp2, Pos p) : 
    UntypedExp_par_ee(uGreater, exp1, exp2, p) {}

  AST *genAST(); 
};

class UntypedExp_GreaterEq:public UntypedExp_par_ee {
public:
  UntypedExp_GreaterEq(UntypedExp *exp1, UntypedExp *exp2, Pos p) : 
    UntypedExp_par_ee(uGreaterEq, exp1, exp2, p) {}

  AST *genAST(); 
};

class UntypedExp_Equal:public UntypedExp_par_ee {
public:
  UntypedExp_Equal(UntypedExp *exp1, UntypedExp *exp2, Pos p) :
    UntypedExp_par_ee(uEqual, exp1, exp2, p) {}

  AST *genAST(); 
};

class UntypedExp_NotEqual:public UntypedExp_par_ee {
public:
  UntypedExp_NotEqual(UntypedExp *exp1, UntypedExp *exp2, Pos p) :
    UntypedExp_par_ee(uNotEqual, exp1, exp2, p) {}

  AST *genAST(); 
};

class UntypedExp_Union:public UntypedExp_par_ee {
public:
  UntypedExp_Union(UntypedExp *exp1, UntypedExp *exp2, Pos p) :
    UntypedExp_par_ee(uUnion, exp1, exp2, p) {}

  AST *genAST();
};

class UntypedExp_Inter:public UntypedExp_par_ee {
public:
  UntypedExp_Inter(UntypedExp *exp1, UntypedExp *exp2, Pos p) :
    UntypedExp_par_ee(uInter, exp1, exp2, p) {}

  AST *genAST();
};

class UntypedExp_Setminus:public UntypedExp_par_ee {
public:
  UntypedExp_Setminus(UntypedExp *exp1, UntypedExp *exp2, Pos p) :
    UntypedExp_par_ee(uSetminus, exp1, exp2, p) {}

  AST *genAST();
};

class UntypedExp_Impl:public UntypedExp_par_ee {
public:
  UntypedExp_Impl(UntypedExp *exp1, UntypedExp *exp2, Pos p) :
    UntypedExp_par_ee(uImpl, exp1, exp2, p) {}

  AST *genAST();
};

class UntypedExp_Biimpl:public UntypedExp_par_ee {
public:
  UntypedExp_Biimpl(UntypedExp *exp1, UntypedExp *exp2, Pos p) :
    UntypedExp_par_ee(uBiimpl, exp1, exp2, p) {}

  AST *genAST();
};

class UntypedExp_And:public UntypedExp_par_ee {
public:
  UntypedExp_And(UntypedExp *exp1, UntypedExp *exp2, Pos p) :
    UntypedExp_par_ee(uAnd, exp1, exp2, p) {}

  AST *genAST();
};

class UntypedExp_Or:public UntypedExp_par_ee {
public:
  UntypedExp_Or(UntypedExp *exp1, UntypedExp *exp2, Pos p) :
    UntypedExp_par_ee(uOr, exp1, exp2, p) {}

  AST *genAST();
};

class UntypedExp_Not:public UntypedExp_par_e {
public:
  UntypedExp_Not(UntypedExp *exp, Pos p) :
    UntypedExp_par_e(uNot, exp, p) {}

  AST *genAST();
};


class UntypedExp_Dot:public UntypedExp {
public:
  UntypedExp_Dot(UntypedExp *e, char  *bts, Pos p) :
    UntypedExp(uDot, p), bits(bts), exp(e) {}
  virtual ~UntypedExp_Dot() {delete exp;}

  AST *genAST();

  char *bits;
  UntypedExp *exp;
};

class UntypedExp_Up:public UntypedExp_par_e {
public:
  UntypedExp_Up(UntypedExp *exp, Pos p) :
    UntypedExp_par_e(uUp, exp, p) {}

  AST *genAST();
};

class UntypedExp_Ex0:public UntypedExp_par_npee  {
public:
  UntypedExp_Ex0(VarDeclList *d, 
		 UntypedPragmaList *prags,
		 UntypedExp *exp, Pos p) :
    UntypedExp_par_npee(uEx0, d, prags, exp, p) {}  

  AST *genAST();
};

class UntypedExp_Ex1:public UntypedExp_par_unpee  {
public:
  UntypedExp_Ex1(NameList *univList, 
		 VarDeclList *d,
		 UntypedPragmaList *prags,
		 UntypedExp *exp, Pos p) :
    UntypedExp_par_unpee(uEx1, univList, d, prags, exp, p) {}

  AST *genAST();
};

class UntypedExp_Ex2:public UntypedExp_par_unpee  {
public:
  UntypedExp_Ex2(NameList *univList, 
		 VarDeclList *d, 
		 UntypedPragmaList *prags, 
		 UntypedExp *exp, Pos p) :
    UntypedExp_par_unpee(uEx2, univList, d, prags, exp, p) {}

  AST *genAST();
};

class UntypedExp_All0:public UntypedExp_par_npee  {
public:
  UntypedExp_All0(VarDeclList *d, 
		  UntypedPragmaList *prags, 
		  UntypedExp *exp, Pos p) :
    UntypedExp_par_npee(uAll0, d, prags, exp, p) {}  

  AST *genAST();
};

class UntypedExp_All1:public UntypedExp_par_unpee  {
public:
  UntypedExp_All1(NameList *univList, 
		  VarDeclList *d, 
		  UntypedPragmaList *prags, 
		  UntypedExp *exp, Pos p) :
    UntypedExp_par_unpee(uAll1, univList, d, prags, exp, p) {}

  AST *genAST();
};

class UntypedExp_All2:public UntypedExp_par_unpee  {
public:
  UntypedExp_All2(NameList *univList, 
		  VarDeclList *d, 
		  UntypedPragmaList *prags, 
		  UntypedExp *exp, Pos p) :
    UntypedExp_par_unpee(uAll2, univList, d, prags, exp, p) {}

  AST *genAST();
};

class UntypedExp_Let0:public UntypedExp_par_bpe {
public:
  UntypedExp_Let0(BindExpList *bindList, 
		  UntypedPragmaList *prags, 
		  UntypedExp *exp, Pos p) :
    UntypedExp_par_bpe(uLet0, bindList, prags, exp, p) {}

  AST *genAST();
};

class UntypedExp_Let1:public UntypedExp_par_bpe {
public:
  UntypedExp_Let1(BindExpList *bindList, 
		  UntypedPragmaList *prags, 
		  UntypedExp *exp, Pos p) :
    UntypedExp_par_bpe(uLet1, bindList, prags, exp, p) {}

  AST *genAST();
};

class UntypedExp_Let2:public UntypedExp_par_bpe {
public:
  UntypedExp_Let2(BindExpList *bindList, 
		  UntypedPragmaList *prags, 
		  UntypedExp *exp, Pos p) :
    UntypedExp_par_bpe(uLet2, bindList, prags, exp, p) {}

  AST *genAST();
};

class UntypedExp_Call:public UntypedExp {
public:
  UntypedExp_Call(Name *n, UntypedExpList *el, Pos p) :
    UntypedExp(uCall, p), name(n), expList(el) {}
  virtual ~UntypedExp_Call() {delete name; delete expList;}

  AST *genAST();

  Name *name;
  UntypedExpList *expList;
};

class UntypedExp_Empty:public UntypedExp {
public:
  UntypedExp_Empty(Pos p) :
    UntypedExp(uEmpty, p) {}

  AST *genAST();
};

class UntypedExp_True:public UntypedExp {
public:
  UntypedExp_True(Pos p) :
    UntypedExp(uTrue, p) {}

  AST *genAST();
};

class UntypedExp_False:public UntypedExp {
public:
  UntypedExp_False(Pos p) :
    UntypedExp(uFalse, p) {}

  AST *genAST();
};

class UntypedExp_Root:public UntypedExp {
public:
  UntypedExp_Root(Name *n, Pos p) :
    UntypedExp(uRoot, p), name(n) {}
  ~UntypedExp_Root() {delete name;}

  AST *genAST();

  Name *name;
};

class UntypedExp_RootPred:public UntypedExp {
public:
  UntypedExp_RootPred(UntypedExp *e, NameList *ul, Pos p) :
    UntypedExp(uRootPred, p), exp(e), univList(ul) {}
  virtual ~UntypedExp_RootPred() {delete exp; delete univList;}

  AST *genAST();

  UntypedExp *exp;
  NameList *univList;
};

class UntypedExp_EmptyPred:public UntypedExp_par_e {
public:
  UntypedExp_EmptyPred(UntypedExp *exp, Pos p) :
    UntypedExp_par_e(uEmptyPred, exp, p) {}

  AST *genAST();
};

class UntypedExp_Plus:public UntypedExp_par_ea {
public:
  UntypedExp_Plus(UntypedExp *exp, ArithExp *aexp, Pos p) :
    UntypedExp_par_ea(uPlus, exp, aexp, p) {}
  
  AST *genAST();
};

class UntypedExp_Minus:public UntypedExp_par_ea {
public:
  UntypedExp_Minus(UntypedExp *exp, ArithExp *aexp, Pos p) :
    UntypedExp_par_ea(uMinus, exp, aexp, p) {}
  
  AST *genAST();
};

class UntypedExp_PlusModulo:public UntypedExp_par_eae {
public:
  UntypedExp_PlusModulo(UntypedExp *exp1, 
			ArithExp *aexp, 
			UntypedExp *exp2, Pos p) :
    UntypedExp_par_eae(uPlusModulo, exp1, aexp, exp2, p) {}

  AST *genAST();
};

class UntypedExp_MinusModulo:public UntypedExp_par_eae {
public:
  UntypedExp_MinusModulo(UntypedExp *exp1, 
			 ArithExp *aexp, 
			 UntypedExp *exp2, Pos p) :
    UntypedExp_par_eae(uMinusModulo, exp1, aexp, exp2, p) {}

  AST *genAST();
};


class UntypedExp_Mult:public UntypedExp_par_ea {
public:
  UntypedExp_Mult(UntypedExp *exp, ArithExp *aexp, Pos p) :
    UntypedExp_par_ea(uMult, exp, aexp, p) {} 
 
  AST *genAST();
};

class UntypedExp_Div:public UntypedExp_par_ea {
public:
  UntypedExp_Div(UntypedExp *exp, ArithExp *aexp, Pos p) :
    UntypedExp_par_ea(uDiv, exp, aexp, p) {} 

  AST *genAST();  
};

class UntypedExp_Int:public UntypedExp {
public:
  UntypedExp_Int(int c, Pos p) :
    UntypedExp(uInt, p), n(c) {}

  AST *genAST();
  
  int n;
};

class UntypedExp_Import:public UntypedExp {
public:
  UntypedExp_Import(char *f, ImportMapList *ml, Pos p) :
    UntypedExp(uImport, p), file(f), mapList(ml) {}
  virtual ~UntypedExp_Import() {delete mapList;}

  AST *genAST();
  
  char *file;
  ImportMapList *mapList;
};

class UntypedExp_Export:public UntypedExp {
public:
  UntypedExp_Export(char *f, UntypedExp *e, Pos p) :
    UntypedExp(uExport, p), file(f), exp(e) {}
  virtual ~UntypedExp_Export() {delete exp;}

  AST *genAST();
  
  char *file;
  UntypedExp *exp;
};

class UntypedExp_Prefix:public UntypedExp {
public:
  UntypedExp_Prefix(UntypedExp *e, Pos p) :
    UntypedExp(uPrefix, p), exp(e) {}
  virtual ~UntypedExp_Prefix() {delete exp;}

  AST *genAST();
  
  UntypedExp *exp;
};

class UntypedExp_InStateSpace:public UntypedExp {
public:
  UntypedExp_InStateSpace(UntypedExp *e, NameList *s, Pos p) :
    UntypedExp(uInStateSpace, p), exp(e), ss(s) {}
  virtual ~UntypedExp_InStateSpace() {delete exp; delete ss;}

  AST *genAST();
  
  UntypedExp *exp;
  NameList *ss;
};

////////// Guide declaration //////////////////////////////////////////////////

class GuideFunc {
public:
  GuideFunc(Name *n1, Name *n2, Name *n3) :
    name1(n1), name2(n2), name3(n3) {}
  ~GuideFunc() {delete name1; delete name2; delete name3;}

  Name *name1;
  Name *name2; 
  Name *name3;
};

class GuideFuncList: public DequeGC<GuideFunc*> {};

////////// Universe declaration ///////////////////////////////////////////////

class Univ {
public:
  Univ(Name *n, char *p = NULL, int d = 0) :
    name(n), pos(p), depth(d) {}
  ~Univ() {delete name;}

  Name *name;
  char *pos;
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
  dExecute
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

class Predicate_Macro_Declaration:public Declaration {
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

class Predicate_Declaration:public Predicate_Macro_Declaration {
public:
  Predicate_Declaration(Name *n, ParDeclList *pars, UntypedExp *b, Pos p) :
    Predicate_Macro_Declaration(dPredicate, n, pars, b, p) {}
};

class Macro_Declaration:public Predicate_Macro_Declaration {
public:
  Macro_Declaration(Name *n, ParDeclList *pars, UntypedExp *b, Pos p) :
    Predicate_Macro_Declaration(dMacro, n, pars, b, p) {}
};

class Variable_Declaration:public Declaration {
public:
  Variable_Declaration(VarDeclKind k, NameList *u, 
		       VarDeclList *d, UntypedPragmaList *pgs, Pos p) :
    Declaration(dVariable, p), declKind(k), univs(u), decls(d), prags(pgs) {}
  virtual ~Variable_Declaration() 
  {delete univs; delete decls; delete prags;}

  void genAST(MonaAST &monaAST);
  
  VarDeclKind declKind;
  NameList *univs;
  VarDeclList *decls;
  UntypedPragmaList *prags;
};

class Universe_Declaration:public Declaration {
public:
  Universe_Declaration(UnivList *ul, Pos p) :
    Declaration(dUniverse, p), univList(ul) {}
  virtual ~Universe_Declaration() {delete univList;}
 
  void genAST(MonaAST &monaAST);

  UnivList *univList;
};

class Expression_Declaration:public Declaration {
public:
  Expression_Declaration(UntypedExp *e, Pos p) :
    Declaration(dExpression, p), exp(e) {}
  virtual ~Expression_Declaration() {delete exp;}

  void genAST(MonaAST &monaAST);
  
  UntypedExp *exp;
};

class Execute_Declaration:public Declaration {
public:
  Execute_Declaration(UntypedExp *e, Pos p) :
    Declaration(dExecute, p), exp(e) {}
  virtual ~Execute_Declaration() {delete exp;}

  void genAST(MonaAST &monaAST);
  
  UntypedExp *exp;
};

class Assertion_Declaration:public Declaration {
public:
  Assertion_Declaration(UntypedExp *e, Pos p) :
    Declaration(dAssertion, p), exp(e) {}
  virtual ~Assertion_Declaration() {delete exp;}
 
  void genAST(MonaAST &monaAST);
  
  UntypedExp *exp;
};

class Constant_Declaration:public Declaration {
public:
  Constant_Declaration(Name *n, ArithExp *a, Pos p) :
    Declaration(dConstant, p), name(n), aexp(a) {}
  virtual ~Constant_Declaration() {delete name; delete aexp;}

  void genAST(MonaAST &monaAST);
  
  Name *name;
  ArithExp *aexp;
};

class Guide_Declaration:public Declaration {
public:
  Guide_Declaration(GuideFuncList *g, Pos p) :
    Declaration(dGuide, p), funcList(g) {}
  virtual ~Guide_Declaration() {delete funcList;}

  void genAST(MonaAST &monaAST);
  
  GuideFuncList *funcList;
};

class Default_Declaration:public Declaration {
public:
  Default_Declaration(VarDeclKind k, Name *n, UntypedExp *e, Pos p) :
    Declaration(dDefault, p), type(k), name(n), exp(e) {}
  virtual ~Default_Declaration() {delete name; delete exp;}

  void genAST(MonaAST &monaAST);

  VarDeclKind type;
  Name *name;
  UntypedExp *exp;
};

class LastPos_Declaration:public Declaration {
public:
  LastPos_Declaration(Name *n, Pos p) :
    Declaration(dLastPos, p), name(n) {}
  virtual ~LastPos_Declaration() {delete name;}

  void genAST(MonaAST &monaAST);
  
  Name *name;
};

////////// Main untyped AST ///////////////////////////////////////////////////

class MonaUntypedAST {
public:
  MonaUntypedAST(DeclarationList *hdr, DeclarationList *decls) :
    header(hdr), declarations(decls) {}
  ~MonaUntypedAST() {delete header; delete declarations;}

  MonaAST *typeCheck();

  DeclarationList *header;
  DeclarationList *declarations;
};

#endif
