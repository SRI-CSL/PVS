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

#include <stdlib.h>
#include <stdio.h>
#include "untyped.h"
#include "predlib.h"
#include "env.h"
#include "deque.h"
extern "C" {
#include "../GTA/gta.h"
}

extern SymbolTable symbolTable;
extern PredicateLib predicateLib;
extern Options options; 
extern int numTypes;
extern Guide_Declaration *guide_declaration;

bool inDefault = false;
bool predMacroEncountered = false;
bool anyUniverses = false;

///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//       Type-error reporting                                                //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

void
TypeError(String str, Pos &p)
{
  cout << "Error";
  if (p.fileName) {
    cout << " in file"; 
    p.printsource();
  }
  cout << "\n" << str << "\n" << "Execution aborted\n";  
  exit(-1);
}

///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//       Arithmetic expressions                                              //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

int 
ArithExp_Add::evaluate() 
{ 
  return aexp1->evaluate() + aexp2->evaluate(); 
}

int 
ArithExp_Subtr::evaluate() 
{ 
  return aexp1->evaluate() - aexp2->evaluate(); 
}

int 
ArithExp_Mult::evaluate() 
{ 
  return aexp1->evaluate() * aexp2->evaluate(); 
}

int 
ArithExp_Div::evaluate() 
{ 
  int x = aexp2->evaluate();

  if (x == 0)
    TypeError("Division by zero", pos);

  return aexp1->evaluate() / x; 
}

int 
ArithExp_Integer::evaluate() 
{ 
  return n; 
}

int 
ArithExp_Const::evaluate() 
{
  if (symbolTable.lookupType(name) != Constname)
    TypeError("'" + String(name->str) + "' used as a constant", pos);

  return symbolTable.lookupValue(name);
}

///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//       Types                                                               //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

//////////  Type variant components ///////////////////////////////////////////

ASTComponent*
Component::genAST()
{
  if (symbolTable.exists(name->str))
    TypeError((String) "'" + name->str + "' already defined", pos);
  Ident tid = symbolTable.lookupIdent(type);
  if (symbolTable.lookupType(tid) != Typename)
    TypeError((String) "'" + type->str + "' not a type", type->pos);
  return new ASTComponent(name->str, type->str, pos);
}

ASTComponentList*
ComponentList::genAST()
{
  ASTComponentList *v = new ASTComponentList;
  for (iterator i = begin(); i != end(); i++)
    v->push_back((*i)->genAST());
  return v;
}

//////////  Type variants /////////////////////////////////////////////////////

ASTVariant*
Variant::genAST()
{
  ASTComponentList *c = NULL;
  if (components)
    c = components->genAST();
  if (symbolTable.exists(name->str))
    TypeError((String) "'" + name->str + "' already defined", pos);
  return new ASTVariant(name->str, c, pos);
}

ASTVariantList*
VariantList::genAST()
{
  ASTVariantList *v = new ASTVariantList;
  for (iterator i = begin(); i != end(); i++)
    v->push_back((*i)->genAST());
  return v;
}

//////////  Constant Typed Trees //////////////////////////////////////////////

ASTForm*
ConstNode::genAST(UntypedExp *t, Ident treevarId, Ident typeId)
{
  Name *treevar = new Name(symbolTable.lookupSymbol(treevarId), pos);
  Name *type = new Name(symbolTable.lookupSymbol(typeId), pos);
  UntypedExp_Variant v(t, new UntypedExp_Name(treevar, pos),
		       type, new Name(*variant), pos);
  ASTForm *res = (ASTForm *) v.genAST();
  v.exp1 = NULL; /* don't gc */

  ASTVariantList *vs = symbolTable.lookupTypeVariants(typeId);
  ASTVariantList::iterator i;
  for (i = vs->begin(); i != vs->end(); i++)
    if (variant->str == (*i)->name) 
      break;
  if (i == vs->end())
    TypeError("Variant not found", variant->pos);
  
  if (components) { 
    ConstNodeList::iterator j;
    ASTComponentList::iterator k;
    if (components->size() != (*i)->components->size()) 
      TypeError("Wrong number of components", pos);
    for (j = components->begin(), k = (*i)->components->begin(); 
	 j != components->end(); j++, k++) {
      Ident fresh = symbolTable.insertFresh(Varname1, NULL);
      UntypedExp_Name tt(new Name(symbolTable.lookupSymbol(fresh), pos), pos);
      Name nn((*k)->type, pos);
      ASTForm *c2 = (*j)->genAST(&tt, treevarId, 
				 symbolTable.lookupIdent(&nn));
      Name *type = new Name(symbolTable.lookupSymbol(typeId), pos);
      Name *var = new Name(*variant);
      Name *com = new Name((*k)->name, pos);
      UntypedExp_Succ succ(t, type, var, com, pos);
      ASTForm *c1 = new ASTForm_Equal1(new ASTTerm1_Var1(fresh, pos),
				       (ASTTerm1 *) succ.genAST(), pos);
      succ.exp = NULL; /* don't gc */
      ASTForm *comp = new ASTForm_And(c1, c2, pos);
      comp = new ASTForm_Ex1(symbolTable.allRealUnivs(),
			     new IdentList(fresh), comp, pos); 
      // (can't find any better univlist)
      res = new ASTForm_And(res, comp, pos);
    }
  }   
  
  return res;
}

///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//       Untyped mona expressions                                            //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

//////////  UntypedExp_Name ///////////////////////////////////////////////////

AST* 
UntypedExp_Name::genAST() 
{
  Ident id = symbolTable.lookupIdent(name);

  switch (symbolTable.lookupType(name)) {

  case Varname0:
  case Parname0:
    return new ASTForm_Var0(id, pos);
   
  case Varname1:
  case Parname1:
    return new ASTTerm1_Var1(id, pos);
    
  case Parname2:
  case Varname2:
    return new ASTTerm2_Var2(id, pos);
    
  case VarnameTree:
    return new ASTTerm2_VarTree(id, pos);
    
  case ParnameU:
  case Univname: 
    return new ASTUniv(id, pos);
    
  case Predname: 
    {
      ASTList *empty = new ASTList;

      if (predicateLib.testTypes(id, empty) == tWrongNoParameters)
	TypeError("Wrong number of parameters to predicate '"
		  + String(name->str) + "'", pos);

      return new ASTForm_Call(id, empty, pos);
    }
  
  case Constname:
  default:
    return new ASTTerm1_Int(symbolTable.lookupValue(name), pos);
  }
}
  
//////////  UntypedExp_Sub ////////////////////////////////////////////////////

AST* 
UntypedExp_Sub::genAST() 
{
  AST *T1 = exp1->genAST();
  AST *T2 = exp2->genAST();

  if (T1->order != oTerm2 || T2->order != oTerm2)
    TypeError("Type mismatch at 'sub'", pos);

  return new ASTForm_Sub((ASTTerm2 *) T1, (ASTTerm2 *) T2, pos);
}

//////////  UntypedExp_In /////////////////////////////////////////////////////

AST* 
UntypedExp_In::genAST()        
{
  AST *t1 = exp1->genAST();
  AST *T2 = exp2->genAST();

  if (t1->order != oTerm1 || T2->order != oTerm2)
    TypeError("Type mismatch at 'in'", pos);

  return new ASTForm_In((ASTTerm1 *) t1, (ASTTerm2 *) T2, pos);
}

//////////  UntypedExp_NotIn //////////////////////////////////////////////////

AST* 
UntypedExp_NotIn::genAST()        
{
  AST *t1 = exp1->genAST();
  AST *T2 = exp2->genAST();

  if (t1->order != oTerm1 || T2->order != oTerm2)
    TypeError("Type mismatch at 'notin'", pos);

  return new ASTForm_Notin((ASTTerm1 *) t1, (ASTTerm2 *) T2, pos);
}

//////////  UntypedExp_Min ////////////////////////////////////////////////////

AST* 
UntypedExp_Min::genAST() 
{ 
  if (options.mode == TREE)
    TypeError("'min' only allowed in linear mode", pos);

  AST *T = exp->genAST();

  if (T->order != oTerm2)
    TypeError("Type mismatch at 'min'", pos);

  return new ASTTerm1_Min((ASTTerm2 *) T, pos);
}

//////////  UntypedExp_Max ////////////////////////////////////////////////////

AST* 
UntypedExp_Max::genAST() 
{ 
  if (options.mode == TREE)
    TypeError("'max' only allowed in linear mode", pos);

  AST *T = exp->genAST();

  if (T->order != oTerm2)
    TypeError("Type mismatch at 'max'", pos);

  return new ASTTerm1_Max((ASTTerm2 *) T, pos);
}

//////////  UntypedExp_Set ////////////////////////////////////////////////////

AST* 
UntypedExp_Set::genAST()
{
  ASTList *elements = new ASTList;
  ASTTerm1 *e1 = NULL;
  ASTTerm1 *e2;
  ASTTerm1 *e3 = NULL;

  if (options.mode == TREE && expList->empty())
    TypeError("{} not allowed in tree mode (use empty(T))", pos);

  // convert to a list of intervals and single elements
  UntypedExpList::iterator n = expList->begin(); 
  while (n != expList->end() || e1 != NULL) {

    if (e1 == NULL) {
      e1 = (ASTTerm1 *) (*n)->genAST();
      n++;
    }
    if (n != expList->end()) {
      e2 = (ASTTerm1 *) (*n)->genAST();
      n++;
      if (e2->kind == aInterval) {
	if (n != expList->end()) {
	  e3 = (ASTTerm1 *) (*n)->genAST();
	  n++;
	}
	else 
	  TypeError("Illegal interval", pos);
      }
    }
    else
      e2 = (ASTTerm1 *) NULL;

    if ((e2 != NULL) && (e2->kind == aInterval)) {
      // interval
      if (e1->kind == aInterval || e3->kind == aInterval)
	TypeError("Illegal interval", pos);
      if (e1->order != oTerm1 || e3->order != oTerm1)
	TypeError("Type mismatch in set", pos);
      elements->push_back(new ASTTerm2_Interval(e1, e3, pos));      
      e1 = (ASTTerm1 *) NULL;
      delete e2;
    }
    else {
      // single element
      if (e1->kind == aInterval)
	TypeError("Illegal interval", pos);
      if (e1->order != oTerm1)
	TypeError("Type mismatch in set", pos);
      elements->push_back(e1);
      e1 = e2;
    }
  }
  
  return new ASTTerm2_Set(elements, pos);
}

//////////  UntypedExp_Interval ///////////////////////////////////////////////

AST* 
UntypedExp_Interval::genAST() 
{ 
  return new ASTTerm2_Interval((ASTTerm1 *) NULL, (ASTTerm1 *) NULL, dummyPos);
}
  
//////////  UntypedExp_Less ///////////////////////////////////////////////////

AST* 
UntypedExp_Less::genAST() 
{
  AST *t1 = exp1->genAST();
  AST *t2 = exp2->genAST();

  if (t1->order != oTerm1 || t2->order != oTerm1)
    TypeError("Type mismatch at '<'", pos);

  return new ASTForm_Less((ASTTerm1 *) t1, (ASTTerm1 *) t2, pos);
}

//////////  UntypedExp_LessEq /////////////////////////////////////////////////

AST* 
UntypedExp_LessEq::genAST() 
{
  AST *t1 = exp1->genAST();
  AST *t2 = exp2->genAST();

  if (t1->order != oTerm1 || t2->order != oTerm1)
    TypeError("Type mismatch at '<='", pos);

  return new ASTForm_LessEq((ASTTerm1 *) t1, (ASTTerm1 *) t2, pos);
}

//////////  UntypedExp_Greater ////////////////////////////////////////////////

AST* 
UntypedExp_Greater::genAST() 
{
  AST *t1 = exp1->genAST();
  AST *t2 = exp2->genAST();

  if (t1->order != oTerm1 || t2->order != oTerm1)
    TypeError("Type mismatch at '>'", pos);

  return new ASTForm_Less((ASTTerm1 *) t2, (ASTTerm1 *) t1, pos);
}

//////////  UntypedExp_GreaterEq //////////////////////////////////////////////

AST* 
UntypedExp_GreaterEq::genAST() 
{
  AST *t1 = exp1->genAST();
  AST *t2 = exp2->genAST();

  if (t1->order != oTerm1 || t2->order != oTerm1)
    TypeError("Type mismatch at '>='", pos);

  return new ASTForm_LessEq((ASTTerm1 *) t2, (ASTTerm1 *) t1, pos);
}

//////////  UntypedExp_Equal //////////////////////////////////////////////////

AST* 
UntypedExp_Equal::genAST() 
{
  AST *e1 = exp1->genAST();
  AST *e2 = exp2->genAST();
  AST *result = 0;
  
  if (e1->order == oTerm1 && e2->order == oTerm1)
    result = new ASTForm_Equal1((ASTTerm1 *) e1, (ASTTerm1 *) e2, pos);
  else if (e1->order == oTerm2 && e2->order == oTerm2) 
    result = new ASTForm_Equal2((ASTTerm2 *) e1, (ASTTerm2 *) e2, pos);
  else
    TypeError("Type mismatch at '='", pos);

  return result;
}

//////////  UntypedExp_NotEqual ///////////////////////////////////////////////

AST* 
UntypedExp_NotEqual::genAST() 
{
  AST *e1 = exp1->genAST();
  AST *e2 = exp2->genAST();
  AST *result = 0;
  
  if (e1->order == oTerm1 && e2->order == oTerm1)
    result = new ASTForm_NotEqual1((ASTTerm1 *) e1, (ASTTerm1 *) e2, pos);
  else if (e1->order == oTerm2 && e2->order == oTerm2)
    result = new ASTForm_NotEqual2((ASTTerm2 *) e1, (ASTTerm2 *) e2, pos);
  else 
    TypeError("Type mismatch at '~='", pos);

  return result;
}

//////////  UntypedExp_Union //////////////////////////////////////////////////

AST* 
UntypedExp_Union::genAST()
{
  AST *T1 = exp1->genAST();
  AST *T2 = exp2->genAST();

  if (T1->order != oTerm2 || T2->order != oTerm2)
    TypeError("Type mismatch at 'union'", pos);

  return new ASTTerm2_Union((ASTTerm2 *) T1, (ASTTerm2 *) T2, pos);
}

//////////  UntypedExp_Inter //////////////////////////////////////////////////

AST* 
UntypedExp_Inter::genAST()
{
  AST *T1 = exp1->genAST();
  AST *T2 = exp2->genAST();

  if (T1->order != oTerm2 || T2->order != oTerm2)
    TypeError("Type mismatch at 'inter'", pos);

  return new ASTTerm2_Inter((ASTTerm2 *) T1, (ASTTerm2 *) T2, pos);
}

//////////  UntypedExp_Setminus ///////////////////////////////////////////////

AST*
UntypedExp_Setminus::genAST()
{
  AST *T1 = exp1->genAST();
  AST *T2 = exp2->genAST();

  if (T1->order != oTerm2 || T2->order != oTerm2)
    TypeError("Type mismatch at '\\'", pos);

  return new ASTTerm2_Setminus((ASTTerm2 *) T1, (ASTTerm2 *) T2, pos);
}

//////////  UntypedExp_Impl ///////////////////////////////////////////////////

AST* 
UntypedExp_Impl::genAST() 
{ 
  AST *f1 = exp1->genAST();
  AST *f2 = exp2->genAST();

  if (f1->order != oForm || f2->order != oForm)
    TypeError("Type mismatch at '=>'", pos);

  return new ASTForm_Impl((ASTForm *) f1, (ASTForm *) f2, pos); 
}

//////////  UntypedExp_Biimpl /////////////////////////////////////////////////

AST* 
UntypedExp_Biimpl::genAST() 
{ 
  AST *f1 = exp1->genAST();
  AST *f2 = exp2->genAST();

  if (f1->order != oForm || f2->order != oForm)
    TypeError("Type mismatch at '<=>'", pos);

  return new ASTForm_Biimpl((ASTForm *) f1, (ASTForm *) f2, pos); 
}

//////////  UntypedExp_And ////////////////////////////////////////////////////


AST* 
UntypedExp_And::genAST() 
{ 
  AST *f1 = exp1->genAST();
  AST *f2 = exp2->genAST();

  if (f1->order != oForm || f2->order != oForm)
    TypeError("Type mismatch at '&'", pos);

  return new ASTForm_And((ASTForm *) f1, (ASTForm *) f2, pos); 
}

//////////  UntypedExp_Or /////////////////////////////////////////////////////

AST* 
UntypedExp_Or::genAST() 
{ 
  AST *f1 = exp1->genAST();
  AST *f2 = exp2->genAST();

  if (f1->order != oForm || f2->order != oForm)
    TypeError("Type mismatch at '|'", pos);

  return new ASTForm_Or((ASTForm *) f1, (ASTForm *) f2, pos); 
}

//////////  UntypedExp_Not ////////////////////////////////////////////////////

AST* 
UntypedExp_Not::genAST() 
{ 
  AST *f = exp->genAST();

  if (f->order != oForm)
    TypeError("Type mismatch at '~'", pos);

  return new ASTForm_Not((ASTForm *) f, pos); 
}

//////////  UntypedExp_Dot ////////////////////////////////////////////////////

AST* 
UntypedExp_Dot::genAST()
{
  if (numTypes > 0)
    cout << "Warning: '.' should not be used "
	 << "explicitly if types are defined\n";
  if (options.mode != TREE)
    TypeError("'.' only allowed in tree mode", pos);

  BitList *b = new BitList(bits);
  AST *t = exp->genAST();

  switch (t->order) {
  case oTerm1:
    return new ASTTerm1_Dot((ASTTerm1 *) t, b, pos);
  case oTerm2:
    return new ASTTerm2_Dot((ASTTerm2 *) t, b, pos);
  default:
    TypeError("Type mismatch at '.'", pos);
    exit(-1);
  }
}

//////////  UntypedExp_Up /////////////////////////////////////////////////////

int numImplicitUp = 0; // number of implicit ^'s (generated by m2l-tree)

AST* 
UntypedExp_Up::genAST()
{
  if (numImplicitUp > 0)
    numImplicitUp--;
  else if (numTypes > 0)
    cout << "Warning: '^' should not be used "
	 << "explicitly if types are defined\n";
  if (options.mode != TREE)
    TypeError("'^' only allowed in tree mode", pos);

  AST *t = exp->genAST();

  if (t->order != oTerm1 && t->order != oTerm2)
    TypeError("Type mismatch at '^'", pos);

  if (t->order == oTerm1)
    return new ASTTerm1_Up((ASTTerm1 *) t, pos);
  else
    return new ASTTerm2_Up((ASTTerm2 *) t, pos);
}

//////////  UntypedExp_Ex0 ////////////////////////////////////////////////////

AST* 
UntypedExp_Ex0::genAST()
{
  symbolTable.openLocal();

  IdentList *idents = new IdentList;
  NameList names;
  Ident id;
  VarDeclList::iterator d;

  for (d = nameList->begin(); d != nameList->end(); d++) {
    names.push_back((*d)->name);
    
    id = symbolTable.insertVar((*d)->name, Varname0, NULL, true);
    idents->push_back(id);
    
    if ((*d)->where) {
      AST *f = (*d)->where->genAST();
      if (f->order != oForm)
	TypeError("Type mismatch at 'where' expression", (*d)->pos);
      
      symbolTable.updateRestriction(id, (ASTForm *) f);
    }
  }
  
  symbolTable.checkUniqueness(names);

  AST *f = exp->genAST();
  if (f->order != oForm)
    TypeError("Type mismatch at 'ex0'", pos);

  AST *result = new ASTForm_Ex0(idents, (ASTForm *) f, pos);

  symbolTable.closeLocal();

  names.reset();
  return result;
}

//////////  UntypedExp_Ex1 ////////////////////////////////////////////////////

AST* 
UntypedExp_Ex1::genAST()
{
  symbolTable.openLocal();

  IdentList *idents = new IdentList;
  NameList names;
  Ident id;
  VarDeclList::iterator d;

  for (d = nameList->begin(); d != nameList->end(); d++) {
    names.push_back((*d)->name);
    
    id = symbolTable.insertVar((*d)->name, Varname1, NULL, true);
    idents->push_back(id);
    
    if ((*d)->where) {
      AST *f = (*d)->where->genAST();
      if (f->order != oForm)
	TypeError("Type mismatch at 'where' expression", (*d)->pos);
      
      symbolTable.updateRestriction(id, (ASTForm *) f);
    }
  }
  
  symbolTable.checkUniqueness(names);

  IdentList *univIdents = NULL;
  if (univList)
    univIdents = symbolTable.lookupUnivIdents(*univList);
  else
    univIdents = symbolTable.allRealUnivs();
 
  AST *f = exp->genAST();
  if (f->order != oForm)
    TypeError("Type mismatch at 'ex1'", pos);

  AST *result = new ASTForm_Ex1(univIdents, idents, (ASTForm *) f, pos);

  symbolTable.closeLocal();
  
  names.reset();
  return result;
}

//////////  UntypedExp_Ex2 ////////////////////////////////////////////////////

AST* 
UntypedExp_Ex2::genAST()
{
  symbolTable.openLocal();

  IdentList *idents = new IdentList;
  NameList names;
  Ident id;
  VarDeclList::iterator d;

  for (d = nameList->begin(); d != nameList->end(); d++) {
    names.push_back((*d)->name);
    
    id = symbolTable.insertVar((*d)->name, Varname2, NULL, true);
    idents->push_back(id);
    
    if ((*d)->where) {
      AST *f = (*d)->where->genAST();
      if (f->order != oForm)
	TypeError("Type mismatch at 'where' expression", (*d)->pos);
      
      symbolTable.updateRestriction(id, (ASTForm *) f);
    }
  }
  
  symbolTable.checkUniqueness(names);

  IdentList *univIdents = NULL;
  if (univList)
    univIdents = symbolTable.lookupUnivIdents(*univList);
  else
    univIdents = symbolTable.allRealUnivs();
 
  AST *f = exp->genAST();
  if (f->order != oForm)
    TypeError("Type mismatch at 'ex2'", pos);

  AST *result = new ASTForm_Ex2(univIdents, idents, (ASTForm *) f, pos);

  symbolTable.closeLocal();
  
  names.reset();
  return result;
}

//////////  UntypedExp_All0 ///////////////////////////////////////////////////

AST* 
UntypedExp_All0::genAST()
{
  symbolTable.openLocal();

  IdentList *idents = new IdentList;
  NameList names;
  Ident id;
  VarDeclList::iterator d;

  for (d = nameList->begin(); d != nameList->end(); d++) {
    names.push_back((*d)->name);
    
    id = symbolTable.insertVar((*d)->name, Varname0, NULL, true);
    idents->push_back(id);
    
    if ((*d)->where) {
      AST *f = (*d)->where->genAST();
      if (f->order != oForm)
	TypeError("Type mismatch at 'where' expression", (*d)->pos);
      
      symbolTable.updateRestriction(id, (ASTForm *) f);
    }
  }
  
  symbolTable.checkUniqueness(names);

  AST *f = exp->genAST();
  if (f->order != oForm)
    TypeError("Type mismatch at 'all0'", pos);

  AST *result = new ASTForm_All0(idents, (ASTForm *) f, pos);

  symbolTable.closeLocal();

  names.reset();
  return result;
}

//////////  UntypedExp_All1 ///////////////////////////////////////////////////

AST* 
UntypedExp_All1::genAST()
{
  symbolTable.openLocal();

  IdentList *idents = new IdentList;
  NameList names;
  Ident id;
  VarDeclList::iterator d;

  for (d = nameList->begin(); d != nameList->end(); d++) {
    names.push_back((*d)->name);
    
    id = symbolTable.insertVar((*d)->name, Varname1, NULL, true);
    idents->push_back(id);
    
    if ((*d)->where) {
      AST *f = (*d)->where->genAST();
      if (f->order != oForm)
	TypeError("Type mismatch at 'where' expression", (*d)->pos);
      
      symbolTable.updateRestriction(id, (ASTForm *) f);
    }
  }
  
  symbolTable.checkUniqueness(names);

  IdentList *univIdents = NULL;
  if (univList)
    univIdents = symbolTable.lookupUnivIdents(*univList);
  else
    univIdents = symbolTable.allRealUnivs();
 
  AST *f = exp->genAST();
  if (f->order != oForm)
    TypeError("Type mismatch at 'all1'", pos);

  AST *result = new ASTForm_All1(univIdents, idents, (ASTForm *) f, pos);

  symbolTable.closeLocal();
  
  names.reset();
  return result;
}

//////////  UntypedExp_All2 ///////////////////////////////////////////////////

AST* 
UntypedExp_All2::genAST()
{
  symbolTable.openLocal();

  IdentList *idents = new IdentList;
  NameList names;
  Ident id;
  VarDeclList::iterator d;

  for (d = nameList->begin(); d != nameList->end(); d++) {
    names.push_back((*d)->name);
    
    id = symbolTable.insertVar((*d)->name, Varname2, NULL, true);
    idents->push_back(id);
    
    if ((*d)->where) {
      AST *f = (*d)->where->genAST();
      if (f->order != oForm)
	TypeError("Type mismatch at 'where' expression", (*d)->pos);
      
      symbolTable.updateRestriction(id, (ASTForm *) f);
    }
  }
  
  symbolTable.checkUniqueness(names);

  IdentList *univIdents = NULL;
  if (univList)
    univIdents = symbolTable.lookupUnivIdents(*univList);
  else
    univIdents = symbolTable.allRealUnivs();
 
  AST *f = exp->genAST();
  if (f->order != oForm)
    TypeError("Type mismatch at 'all2'", pos);

  AST *result = new ASTForm_All2(univIdents, idents, (ASTForm *) f, pos);

  symbolTable.closeLocal();
  
  names.reset();
  return result;
}

//////////  UntypedExp_Let0 ///////////////////////////////////////////////////

AST* 
UntypedExp_Let0::genAST()
{
  symbolTable.openLocal();
  
  NameList nl;
  BindExpList::iterator n;
  IdentList *idents = new IdentList;
  Ident id;
  FormList *forms = new FormList;
  IdentList::iterator i;

  for (n = bindList->begin(), i = idents->begin(); 
       n != bindList->end(); n++, i++) {
    AST *t = ((*n)->exp)->genAST();
    if (t->order != oForm) 
      TypeError("Type mismatch at 'let0' in definition of '"
		+ String(((*n)->name)->str) +"'", (*n)->pos);
    forms->push_back((ASTForm* ) t);
  }
 
  for (n = bindList->begin(); n!= bindList->end(); n++) {
    id = symbolTable.insertVar((*n)->name, Varname0, NULL, true);
    idents->push_back(id);
    nl.push_back((*n)->name);
  }
  symbolTable.checkUniqueness(nl);

  AST *t = exp->genAST();
  if (t->order != oForm)
    TypeError("Type mismatch at 'let0' expression", pos);

  AST *result = new ASTForm_Let0(idents, forms, (ASTForm *) t, pos);

  symbolTable.closeLocal();

  nl.reset();
  return result;
}

//////////  UntypedExp_Let1 ///////////////////////////////////////////////////

AST* 
UntypedExp_Let1::genAST()
{
  symbolTable.openLocal();

  NameList nl;
  BindExpList::iterator n;
  IdentList *idents = new IdentList;
  Ident id;
  Term1List *terms = new Term1List;
  IdentList::iterator i;

  for (n = bindList->begin(), i = idents->begin(); 
       n != bindList->end(); n++, i++) {
    AST *t = ((*n)->exp)->genAST();
    if (t->order != oTerm1) 
      TypeError("Type mismatch at 'let1' in definition of '"
		+ String(((*n)->name)->str) +"'", (*n)->pos);
    terms->push_back((ASTTerm1 *) t);
  }

  for (n = bindList->begin(); n!= bindList->end(); n++) {
    id = symbolTable.insertVar((*n)->name, Varname1, NULL, true);
    idents->push_back(id);
    nl.push_back((*n)->name);
  }
  symbolTable.checkUniqueness(nl);
  
  AST *t = exp->genAST();
  if (t->order != oForm)
    TypeError("Type mismatch at 'let1' expression", pos);

  AST *result = new ASTForm_Let1(idents, terms, (ASTForm *) t, pos);

  symbolTable.closeLocal();

  nl.reset();
  return result;
}

//////////  UntypedExp_Let2 ///////////////////////////////////////////////////

AST* 
UntypedExp_Let2::genAST()
{
  symbolTable.openLocal();
  
  NameList nl;
  BindExpList::iterator n;
  IdentList *idents = new IdentList;
  Ident id;
  Term2List *terms = new Term2List;
  IdentList::iterator i;

  for (n = bindList->begin(), i = idents->begin(); 
       n != bindList->end(); n++, i++) {
    AST *t = ((*n)->exp)->genAST();
    if (t->order != oTerm2) 
      TypeError("Type mismatch at 'let2' in definition of '"
		+ String(((*n)->name)->str) +"'", (*n)->pos);
    terms->push_back((ASTTerm2 *) t);
  }

  for (n = bindList->begin(); n!= bindList->end(); n++) {
    id = symbolTable.insertVar((*n)->name, Varname2, NULL, true); 
    idents->push_back(id);
    nl.push_back((*n)->name);
  }
  symbolTable.checkUniqueness(nl);

  AST *t = exp->genAST();
  if (t->order != oForm)
    TypeError("Type mismatch at 'let2' expression", pos);

  AST *result = new ASTForm_Let2(idents, terms, (ASTForm *) t, pos);

  symbolTable.closeLocal();

  nl.reset();
  return result;
}

//////////  UntypedExp_Call ///////////////////////////////////////////////////

AST*
UntypedExp_Call::genAST()
{
  if (strcmp(name->str, "pconst") == 0) { // hack -- make generic mechanism!!
    if (options.mode == TREE)
      TypeError("'pconst' (currently) only allowed in linear mode", pos);
    if (expList->size() != 1)
      TypeError("Wrong number of arguments to function 'pconst'", pos);
    AST *arg = (expList->get(0))->genAST();
    if (arg->kind != aInt)
      TypeError("Wrong type of parameter 1 at function 'pconst'", pos);
    int value = ((ASTTerm1_Int *) arg)->value();
    if (value < 0)
      TypeError("Argument to 'presb' negative", pos);
    delete arg;
    return new ASTTerm2_PresbConst(value, pos);
  }
  
  if (symbolTable.lookupType(name) != Predname)
    TypeError("'" + String(name->str) + 
	      "' used as a predicate or function", pos);
  
  ASTList *par = new ASTList;
  if (expList)
    for (UntypedExpList::iterator i = expList->begin(); 
	 i != expList->end(); i++)
      par->push_back((*i)->genAST());
  
  Ident id = symbolTable.lookupIdent(name);
  int no;
  switch (predicateLib.testTypes(id, par, &no)) {

  case tWrongNoParameters:
    TypeError("Wrong number of parameters to predicate '"
	      + String(name->str) + "'", pos);

  case tWrongParameterType:
    char t[10];
    sprintf(t, "%i",no);
    TypeError("Wrong type of parameter " + (String) t + " at predicate '"
	      + String(name->str) + "'", pos);

  case tOK: ;
  }

  return new ASTForm_Call(id, par, pos);
}

//////////  UntypedExp_Empty //////////////////////////////////////////////////

AST* 
UntypedExp_Empty::genAST() 
{ 
  if (options.mode == TREE) 
    TypeError("'empty'-term only allowed in linear mode "
	      "(use predicate version", pos);

  return new ASTTerm2_Empty(pos); 
}

//////////  UntypedExp_True ///////////////////////////////////////////////////

AST* 
UntypedExp_True::genAST() 
{ 
  return new ASTForm_True(pos); 
}

//////////  UntypedExp_False //////////////////////////////////////////////////

AST* 
UntypedExp_False::genAST() 
{ 
  return new ASTForm_False(pos); 
}

//////////  UntypedExp_Root ///////////////////////////////////////////////////

AST* 
UntypedExp_Root::genAST() 
{
  if (options.mode != TREE)
    TypeError("'root' only allowed in tree mode", pos);

  Ident u = 0;

  if (name) {
    switch (symbolTable.lookupType(name)) {
    case Varname1:
    case Varname2:
    case VarnameTree:
    case Univname:
    case Parname1:  
    case Parname2:
    case ParnameU:
      u = symbolTable.lookupIdent(name);
      break;
    default:
      TypeError("Illegal argument to 'root'", pos);
    }
  }
  else if (!anyUniverses)
    u = -1;
  else
    TypeError("Argument to 'root' is missing", pos);

  return new ASTTerm1_Root(u, pos);
}

//////////  UntypedExp_RootPred ///////////////////////////////////////////////

AST* 
UntypedExp_RootPred::genAST() 
{
  if (options.mode != TREE)
    TypeError("'root' only allowed in tree mode", pos);

  AST *t = exp->genAST();

  if (t->order != oTerm1)
    TypeError("Type mismatch at 'root'", pos);

  IdentList *ul = symbolTable.lookupUnivIdents(*univList);

  return new ASTForm_RootPred((ASTTerm1 *) t, ul, pos);
}

//////////  UntypedExp_EmptyPred //////////////////////////////////////////////

AST* 
UntypedExp_EmptyPred::genAST() 
{
  AST *f = exp->genAST();

  if (f->order != oTerm2)
    TypeError("Type mismatch at 'empty'", pos);

  return new ASTForm_EmptyPred((ASTTerm2 *) f, pos);
}

//////////  UntypedExp_Plus ///////////////////////////////////////////////////

AST* 
UntypedExp_Plus::genAST()
{
  if (options.mode == TREE)
    TypeError("'+' only allowed in linear mode", pos);

  AST *e = exp->genAST();
  int i = aexp->evaluate();

  if (e->kind == aInt)
    return new ASTTerm1_Int(((ASTTerm1_Int *) e)->value() + i, 
			    pos);

  if (e->order != oTerm1 && e->order != oTerm2)
    TypeError("Type mismatch at '+'", pos);

  if (i == 0)
    return e;
  else if (i > 0) {
    if (e->order == oTerm1)
      return new ASTTerm1_Plus((ASTTerm1 *) e, i, pos);
    else
      return new ASTTerm2_Plus((ASTTerm2 *) e, i, pos);
  }
  else {
    if (e->order == oTerm1)
      return new ASTTerm1_Minus((ASTTerm1 *) e, -i, pos);
    else
      return new ASTTerm2_Minus((ASTTerm2 *) e, -i, pos);
  }
}

//////////  UntypedExp_Minus //////////////////////////////////////////////////

AST* 
UntypedExp_Minus::genAST()
{
  if (options.mode == TREE)
    TypeError("'-' only allowed in linear mode", pos);

  AST *e = exp->genAST();
  int i = aexp->evaluate();

  if (e->kind == aInt)
    return new ASTTerm1_Int(((ASTTerm1_Int *) e)->value() - i, pos);

  if (e->order != oTerm1 && e->order != oTerm2)
    TypeError("Type mismatch at '-'", pos);

  if (i == 0)
    return e;
  else if (i > 0) {
    if (e->order == oTerm1)
      return new ASTTerm1_Minus((ASTTerm1 *) e, i, pos);
    else 
      return new ASTTerm2_Minus((ASTTerm2 *) e, i, pos);
  }
  else {
    if (e->order == oTerm1)
      return new ASTTerm1_Plus((ASTTerm1 *) e, -i, pos);
    else 
      return new ASTTerm2_Plus((ASTTerm2 *) e, -i, pos);
  }
}

//////////  UntypedExp_PlusModulo /////////////////////////////////////////////

AST* 
UntypedExp_PlusModulo::genAST()
{
  if (options.mode == TREE)
    TypeError("'+%' only allowed in linear mode", pos);

  AST *e1 = exp1->genAST();
  AST *e2 = exp2->genAST();
  int i = aexp->evaluate();

  if (e1->order != oTerm1 || e2->order != oTerm1)
    TypeError("Type mismatch at '+%'", pos);

  if (i >= 0)
    return new ASTTerm1_PlusModulo((ASTTerm1 *) e1, i, (ASTTerm1 *) e2, pos);
  else
    return new ASTTerm1_MinusModulo((ASTTerm1 *) e1, -i, (ASTTerm1 *) e2, pos);
}

//////////  UntypedExp_MinusModulo ////////////////////////////////////////////

AST* 
UntypedExp_MinusModulo::genAST()
{
  if (options.mode == TREE)
    TypeError("'-%' only allowed in linear mode", pos);

  AST *e1 = exp1->genAST();
  AST *e2 = exp2->genAST();
  int i = aexp->evaluate();

  if (e1->order != oTerm1 || e2->order != oTerm1)
    TypeError("Type mismatch at '-%'", pos);

  if (i >= 0) 
    return new ASTTerm1_MinusModulo((ASTTerm1 *) e1, i, (ASTTerm1 *) e2, pos);
  else 
    return new ASTTerm1_PlusModulo((ASTTerm1 *) e1, -i, (ASTTerm1 *) e2, pos);
}

//////////  UntypedExp_Mult ///////////////////////////////////////////////////

AST* 
UntypedExp_Mult::genAST()
{
  if (options.mode == TREE) 
    TypeError("'*' only allowed in linear mode", pos);

  AST *e = exp->genAST();
  int i = aexp->evaluate();

  if (e->kind != aInt) 
    TypeError("Type mismatch at '*'", pos);
  
  return new ASTTerm1_Int(((ASTTerm1_Int *) e)->value()  *i, pos);
}

//////////  UntypedExp_Div ////////////////////////////////////////////////////

AST* 
UntypedExp_Div::genAST()
{
  if (options.mode == TREE) 
    TypeError("'/' only allowed in linear mode", pos);

  AST *e = exp->genAST();
  int i = aexp->evaluate();
    
  if (i == 0)
    TypeError("Division by zero", pos);

  if (e->kind != aInt)
    TypeError("Type mismatch at '/'", pos);
  
  return new ASTTerm1_Int(((ASTTerm1_Int *) e)->value() / i, pos);
}

//////////  UntypedExp_Int ////////////////////////////////////////////////////

AST* 
UntypedExp_Int::genAST()
{
  if (options.mode == TREE)
    TypeError("Integer only allowed in linear mode", pos);

  return new ASTTerm1_Int(n, pos);
}

//////////  UntypedExp_Import /////////////////////////////////////////////////

AST* 
UntypedExp_Import::genAST()
{
  Deque<char*> *fileVars = new Deque<char*>;
  IdentList *idents = new IdentList;

  ImportMapList::iterator i;
  for (i = mapList->begin(); i != mapList->end(); i++) {
    char *fileVar = (*i)->name1->str;
    Ident id = symbolTable.lookupIdent((*i)->name2);

    Deque<char*>::iterator j1;
    for (j1 = fileVars->begin(); j1 != fileVars->end(); j1++)
      if (fileVar == *j1)
	TypeError("Variable name already used in 'import' mapping", 
		  (*i)->name1->pos);
    fileVars->push_back(fileVar);

    IdentList::iterator j2;
    for (j2 = idents->begin(); j2 != idents->end(); j2++)
      if (id == *j2)
	TypeError("Variable name already used in 'import' mapping", 
		  (*i)->name2->pos);
    idents->push_back(id);
  }
  
  return new ASTForm_Import(file, fileVars, idents, pos); 
}

//////////  UntypedExp_Export /////////////////////////////////////////////////

AST* 
UntypedExp_Export::genAST()
{
  AST *f = exp->genAST();

  if (f->order != oForm)
    TypeError("Type mismatch at 'export'", pos);

  return new ASTForm_Export((ASTForm *) f, file, pos); 
}
 

//////////  UntypedExp_Prefix /////////////////////////////////////////////////

AST* 
UntypedExp_Prefix::genAST()
{
  if (options.mode == TREE)
    TypeError("'prefix' only allowed in linear mode", pos);

  AST *f = exp->genAST();

  if (f->order != oForm)
    TypeError("Type mismatch at 'prefix'", pos);

  return new ASTForm_Prefix((ASTForm *) f, pos); 
}

//////////  UntypedExp_InStateSpace ///////////////////////////////////////////

AST* 
UntypedExp_InStateSpace::genAST()
{
  if (numTypes > 0)
    cout << "Warning: 'in_state_space' should not be used "
	 << "explicitly if types are defined\n";
  if (options.mode != TREE)
    TypeError("'in_state_space' only allowed in tree mode", pos);

  AST *t = exp->genAST();

  IdentList *ssid = new IdentList;
  for (NameList::iterator i = ss->begin(); i != ss->end(); i++) {
    ssid->insert(symbolTable.lookupIdent(*i));
    
    if ((t->order != oTerm1 && t->order != oTerm2) || 
	symbolTable.lookupType(*i) != Statespacename)
      TypeError("Type mismatch at 'in_state_space'", pos);
  }    
  ssid->sort();
  if (t->order == oTerm1)
    return new ASTForm_InStateSpace1((ASTTerm1 *) t, ssid, pos); 
  else
    return new ASTForm_InStateSpace2((ASTTerm2 *) t, ssid, pos); 
}

//////////  UntypedExp_Succ ///////////////////////////////////////////////////

AST* 
UntypedExp_Succ::genAST()
{
  if (numTypes == 0)
    TypeError("'succ' only allowed when using types", pos);

  Ident typeId = symbolTable.lookupIdent(type);
  AST *t = exp->genAST();
  if (t->order != oTerm1 || symbolTable.lookupType(typeId) != Typename)
    TypeError("Type mismatch at 'succ'", pos);

  // succ(p,A,a1,x) --> p.PATH<A.a1>PATH<A.a1.x>
  ASTVariantList *v = symbolTable.lookupTypeVariants(typeId);
  ASTVariantList::iterator i;
  for (i = v->begin(); i != v->end(); i++)
    if (strcmp((*i)->name, variant->str)==0)
      break;
  if (i == v->end())
    TypeError((String) "Variant '" + variant->str + "' not found", pos);
  BitList *bits = new BitList(*(*i)->path);
  ASTComponentList *c = (*i)->components;
  ASTComponentList::iterator j = 0;
  if (c)
    for (j = c->begin(); j != c->end(); j++)
      if (strcmp((*j)->name, component->str)==0)
	break;
  if (!c || j == c->end())
    TypeError((String) "Component '" + component->str + "' not found", pos);
  bits->append((*j)->path);

  return new ASTTerm1_Dot((ASTTerm1 *) t, bits, pos);
}

//////////  UntypedExp_WellFormedTree /////////////////////////////////////////

AST* 
UntypedExp_WellFormedTree::genAST()
{
  if (numTypes == 0)
    TypeError("'well_formed_tree' predicate only allowed when using types", pos);

  AST *t = exp->genAST();
  if (t->order != oTerm2)
    TypeError("Type mismatch at 'well_formed_tree'", pos);

  return new ASTForm_WellFormedTree((ASTTerm2 *) t, pos);
}

//////////  UntypedExp_Type ///////////////////////////////////////////////////

AST* 
UntypedExp_Type::genAST()
{
  if (numTypes == 0)
    TypeError("'type' only allowed when using types", pos);

  Ident typeId = symbolTable.lookupIdent(type);
  AST *t = exp->genAST();
  if (t->order != oTerm1 || symbolTable.lookupType(typeId) != Typename)
    TypeError("Type mismatch at 'type'", pos);

  // type(p,A) --> in_state_space(p, STATESPACES<A>)
  return new ASTForm_InStateSpace1((ASTTerm1 *) t,
				   symbolTable.lookupTypeStatespaces(typeId), 
				   pos);
}

//////////  UntypedExp_SomeType ///////////////////////////////////////////////

AST* 
UntypedExp_SomeType::genAST()
{
  if (numTypes == 0)
    TypeError("'sometype' only allowed when using types", pos);

  AST *t = exp->genAST();
  if (t->order != oTerm1 && t->order != oTerm2)
    TypeError("Type mismatch at 'sometype'", pos);

  return new ASTForm_SomeType((ASTTerm *) t, pos);
}

//////////  UntypedExp_Variant ////////////////////////////////////////////////

AST* 
UntypedExp_Variant::genAST()
{
  if (numTypes == 0)
    TypeError("'variant' only allowed when using types", pos);

  Ident typeId = symbolTable.lookupIdent(type);
  AST *t1 = exp1->genAST();
  AST *t2 = exp2->genAST();
  if (t1->order != oTerm1 || t2->order != oTerm2 ||
      symbolTable.lookupType(typeId) != Typename)
    TypeError("Type mismatch at 'variant'", pos);
  
  // variant(p,T,A,a1) --> p.PATH<A.a1> in T & type(p,A)
  ASTVariantList *v = symbolTable.lookupTypeVariants(typeId);
  ASTVariantList::iterator i;
  for (i = v->begin(); i != v->end(); i++)
    if (strcmp((*i)->name, variant->str)==0)
      break;
  if (i == v->end())
    TypeError((String) "Variant '" + variant->str + "' not found", pos);
  if (!(*i)->path)
    TypeError((String) "Type '" + type->str + 
	      "' not occuring in any universe", pos);
  BitList *bits = new BitList(*(*i)->path);
  
  UntypedExp_Type ttype(exp1, type, pos);
  
  AST *res = new ASTForm_And
    (new ASTForm_In(new ASTTerm1_Dot((ASTTerm1 *) t1, bits, pos), 
		    (ASTTerm2 *) t2, pos),
     (ASTForm *) ttype.genAST(), pos);
  
  ttype.exp = 0;
  ttype.type = 0;
  return res;
}

//////////  UntypedExp_ConstTree //////////////////////////////////////////////

AST* 
UntypedExp_ConstTree::genAST()
{
  if (numTypes == 0)
    TypeError("'const_tree' predicate only allowed when using types", pos);

  Ident typeId = symbolTable.lookupIdent(type);
  if (symbolTable.lookupType(typeId) != Typename)
    TypeError("Type name expected", type->pos);
  
  AST *trash = exp->genAST();
  if (trash->order != oTerm1)
    TypeError("Type mismatch at 'const_tree'", exp->pos);
  delete trash;

  Ident fresh = symbolTable.insertFresh(Varname2, symbolTable.allRealUnivs());
  // (can't find any better univlist)
  ASTForm *eq = 
    new ASTForm_Equal1(new ASTTerm1_TreeRoot(new ASTTerm2_Var2(fresh, pos), 
					     pos),
		       (ASTTerm1 *) exp->genAST(), pos);
  ASTForm *f = new ASTForm_And(node->genAST(exp, fresh, typeId), eq, pos);
  
  return new ASTTerm2_Formula(fresh, f, pos);
}

//////////  UntypedExp_TreeRoot ///////////////////////////////////////////////

AST* 
UntypedExp_TreeRoot::genAST()
{
  if (numTypes == 0)
    TypeError("'tree_root' predicate only allowed when using types", pos);

  AST *T = exp->genAST();

  if (T->order != oTerm2)
    TypeError("Type mismatch at 'tree_root'", pos);

  return new ASTTerm1_TreeRoot((ASTTerm2 *) T, pos);
}

//////////  UntypedExp_Restrict ///////////////////////////////////////////////

AST* 
UntypedExp_Restrict::genAST()
{
  AST *T = exp->genAST();
  
  if (T->order != oForm)
    TypeError("Type mismatch at 'restrict'", pos);
  
  return new ASTForm_Restrict((ASTForm *) T, pos);
}

///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//       Declarations                                                        //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

//////////  Predicate_Macro_Declaration ///////////////////////////////////////
  
void 
Predicate_Macro_Declaration::genAST(MonaAST &)
{
  symbolTable.openLocal();

  IdentList *freeIdents = new IdentList;
  IdentList *formalIdents = new IdentList;
  IdentList *boundIdents = new IdentList;
  Ident ident;

  ParDeclList::iterator d;
  MonaTypeTag parKind = (MonaTypeTag) -1;
  for (d = parameters->begin(); d != parameters->end(); d++) {
    switch ((*d)->kind) { 
    case pPar0:
      parKind = Parname0;
      break;
    case pPar1:
      parKind = Parname1;
      break;
    case pPar2:
      parKind = Parname2;
      break;
    case pParU:
      parKind = ParnameU;
      break;
    case pPar:
      if (parKind == -1)
	TypeError("Parameter type missing in declaration of '" 
		  + String(name->str) + "'", (*d)->pos);
      break;
    }

    if ((*d)->where && parKind != Parname1 && parKind != Parname2)
	TypeError("'where' only allowed at var1/var2", (*d)->pos);

    ident = symbolTable.insertVar((*d)->name, parKind, NULL, true); //univs??????
    
    formalIdents->push_back(ident);
    boundIdents->push_back(ident);

    if ((*d)->where) {
      AST *f = (*d)->where->genAST(); 
      // (restriction shouldn't be allowed to access other parameter vars.)
      if (f->order != oForm)
	TypeError("Type mismatch at 'where' expression", (*d)->pos);
      symbolTable.updateRestriction(ident, (ASTForm *) f);
    }

    // update frees with restriction frees
    Ident d;
    ASTForm *fil = symbolTable.getRestriction(ident, &d);
    if (fil) {
      if (d != -1)
	boundIdents->insert(d); // skip the defaultwhere formal      
      fil->freeVars(freeIdents, boundIdents);
    }
  }  

  AST *f = body->genAST();

  if (f->order != oForm)
    TypeError("Type mismatch at declaration of '" + 
	      String(name->str) + "'", pos);

  ((ASTForm *) f)->freeVars(freeIdents, boundIdents);

  symbolTable.closeLocal();

  int id = symbolTable.insertPred(name);

  predicateLib.insert(formalIdents, freeIdents, boundIdents,
		      (ASTForm *) f, kind == dMacro, id, source);

  predMacroEncountered = true;
}
 
//////////  Variable_Declaration //////////////////////////////////////////////

void 
Variable_Declaration::genAST(MonaAST &monaAST)
{
  VarDeclList::iterator d;
  for (d = decls->begin(); d != decls->end(); d++) {
    
    MonaTypeTag t;
    switch (declKind) {
    case vVar0: 
      t = Varname0;
      break;
    case vVar1:
      t = Varname1;
      break;
    case vVar2:
      t = Varname2;
      break;
    case vTree:
      if (numTypes == 0)
	TypeError("Tree declarations illegal when no types declared", pos);
      t = VarnameTree;
      break;
    default:
      invariant(false);
    }
    
    IdentList *u;
    switch (declKind) {
    case vVar0: 
      u = NULL;
      break;
    case vVar1:
    case vVar2:
    case vTree:
      if (univs)
	u = symbolTable.lookupUnivIdents(*univs);
      else
	u = symbolTable.allRealUnivs();
      break;
    default:
      invariant(false);
    }
    
    Ident id = symbolTable.insertVar((*d)->name, t, u);
    monaAST.globals.insert(id);
    
    if ((*d)->where) {
      AST *f = (*d)->where->genAST();
      if (f->order != oForm)
	TypeError("Type mismatch at 'where' expression", (*d)->pos);

      symbolTable.updateRestriction(id, (ASTForm *) f);
    }
  }
}
 
//////////  UntypedExp_LastPos ////////////////////////////////////////////////

void
LastPos_Declaration::genAST(MonaAST &monaAST)
{
  if (options.mode == TREE)
    TypeError("'lastpos' not allowed in tree mode (use 'allpos')", pos);
  if (monaAST.lastPosVar != -1 || monaAST.allPosVar != -1)
    TypeError("'lastpos' or 'allpos' already declared", pos);

  monaAST.lastPosVar = symbolTable.lookupIdent(name);

  if (symbolTable.lookupType(name) != Varname1)
    TypeError("Type mismatch at 'lastpos'", pos);
}
 
//////////  UntypedExp_AllPos /////////////////////////////////////////////////

void
AllPos_Declaration::genAST(MonaAST &monaAST)
{
  if (monaAST.lastPosVar != -1 || monaAST.allPosVar != -1)
    TypeError("'lastpos' or 'allpos' already declared", pos);

  monaAST.allPosVar = symbolTable.lookupIdent(name);

  if (symbolTable.lookupType(name) != Varname2)
    TypeError("Type mismatch at 'allpos'", pos);
}
 
//////////  Universe_Declaration //////////////////////////////////////////////

void 
Universe_Declaration::genAST(MonaAST &monaAST)
{
  if (options.mode != TREE) {
    cout << "Warning: 'universe' only used in tree mode\n";
    return;
  }

  UnivList::iterator i;
  for (i = univList->begin(); i != univList->end(); i++) {
    if (numTypes > 0 && (*i)->type) {
      Ident t = symbolTable.lookupIdent((*i)->type);
      if (symbolTable.lookupType(t) != Typename)
	TypeError((String) "'" + (*i)->type->str + 
		  "' not declared as a type", (*i)->type->pos);
      symbolTable.insertUniv((*i)->name, t);
    }
    else if (!guide_declaration && (*i)->pos)
      TypeError("Universe position illegal when no guide declared", pos);
    else if (guide_declaration && !((*i)->pos))
      TypeError("Universe position missing", pos);
    else if ((*i)->type) 
      TypeError("Illegal universe position", pos);
    else if (numTypes > 0 && !(*i)->type)
      TypeError("Universe type missing", pos);
    else
      symbolTable.insertUniv((*i)->name, (*i)->pos);
  }
}

//////////  Expression_Declaration ////////////////////////////////////////////

void 
Expression_Declaration::genAST(MonaAST &monaAST)
{
  AST *f = exp->genAST();

  if (f->order != oForm)
    TypeError("Type mismatch at expression", pos);

  monaAST.formula = new ASTForm_And(monaAST.formula, (ASTForm *) f, dummyPos);
}
 
//////////  Verify_Declaration ////////////////////////////////////////////////

void 
Verify_Declaration::genAST(MonaAST &monaAST)
{
  AST *f = exp->genAST();

  if (f->order != oForm)
    TypeError("Type mismatch at expression", pos);

  monaAST.verifyformlist.push_back((ASTForm *) f);
  if (!title)
    title = "<untitled>";
  monaAST.verifytitlelist.push_back(title);
}
 
//////////  Execute_Declaration ///////////////////////////////////////////////

void 
Execute_Declaration::genAST(MonaAST &monaAST)
{
  AST *f = exp->genAST();

  if (f->order != oForm)
    TypeError("Type mismatch at expression", pos);

  monaAST.formula = new ASTForm_IdLeft(monaAST.formula, (ASTForm *) f, dummyPos);
}
 
//////////  Assertion_Declaration /////////////////////////////////////////////

void 
Assertion_Declaration::genAST(MonaAST &monaAST)
{
  AST *f = exp->genAST();

  if (f->order != oForm)
    TypeError("Type mismatch at 'assert'", pos);

  monaAST.assertion = new ASTForm_And(monaAST.assertion, (ASTForm *) f, 
				      dummyPos);
}
 
//////////  Constant_Declaration //////////////////////////////////////////////

void 
Constant_Declaration::genAST(MonaAST&)
{
  symbolTable.insertConst(name, aexp->evaluate());
}
 
//////////  Guide_Declaration /////////////////////////////////////////////////

void 
Guide_Declaration::genAST(MonaAST &monaAST)
{
  symbolTable.openLocal();
  
  IdentList *univs = symbolTable.allUnivs();
  if (univs && !univs->empty())
    TypeError("Universes declared before guide", pos);
  if (univs)
    delete univs;

  if (guide_declaration)
    TypeError("Guide already declared", pos);
  if (numTypes > 0)
    TypeError("Cannot mix recursive types and guide declaration", pos);
  if (options.mode != TREE)
    TypeError("Guide not allowed in linear mode", pos);

  GuideFuncList::iterator i;
  for (i = funcList->begin(); i != funcList->end(); i++)
    symbolTable.insertStatespace((*i)->name1);

  guide_declaration = this;

  symbolTable.closeLocal();
}
 
//////////  Default_Declaration ///////////////////////////////////////////////

bool default1 = false, default2 = false;

void 
Default_Declaration::genAST(MonaAST &)
{
  if (predMacroEncountered)
    TypeError("'defaultwhere' declarations must come before all "
	      "predicate and macro declarations", pos);

  MonaTypeTag t;
  switch (type) {
  case vVar1:
    t = Varname1;
    if (default1)
      TypeError("'defaultwhere1' already declared", pos);
    default1 = true;
    break;
  case vVar2:
    t = Varname2;
    if (default2)
      TypeError("'defaultwhere2' already declared", pos);
    default2 = true;
    break;
  default: 
    invariant(false);
  }

  symbolTable.openLocal();
  Ident id = symbolTable.insertVar(name, t, NULL, true);

  inDefault = true;
  AST *f = exp->genAST();
  inDefault = false;
  
  if (f->order != oForm)
    TypeError("Type mismatch at 'defaultwhere'", pos);

  symbolTable.closeLocal();

  symbolTable.setDefaultRestriction(t, (ASTForm *) f, id);
}

//////////  Type_Declaration //////////////////////////////////////////////////

void
Type_Declaration::genAST(MonaAST &monaAST)
{
  if (options.mode != TREE)
    TypeError("Types in linear mode not implemented", pos);
  if (guide_declaration)
    TypeError("Cannot mix type and guide declarations", pos);

  Ident id = symbolTable.lookupIdent(name);
  ASTVariantList *v = variants->genAST();
  symbolTable.setTypeVariants(id, v);

  symbolTable.setTypeNumber(id, initTreetype(name->str, v->size()));
  for (ASTVariantList::iterator i = v->begin(); i != v->end(); i++) {
    ASTComponentList *c = (*i)->components;
    initTreetypeVariant((*i)->name, c ? c->size() : 0);
    if (c)
      for (ASTComponentList::iterator j = c->begin(); j != c->end(); j++) 
	setTreetypeComponent((*j)->name, (*j)->type);
  }
}

void
Type_Declaration::checkReachable()
{
  if (!symbolTable.lookupTypeReachable(symbolTable.lookupIdent(name)))
    TypeError("Type not occuring in any universe", name->pos);
}

//////////  MonaUntypedAST ////////////////////////////////////////////////////

MonaAST *
MonaUntypedAST::typeCheck()
{
  MonaAST *monaAST = 
    new MonaAST(new ASTForm_True(dummyPos),  // initial formula
		new ASTForm_True(dummyPos)); // initial assertion

  // desugar m2l-tree/str
  if (options.m2l) {
    UntypedExp *exp, *exp1, *exp2;
    UntypedExpList *elist;
    VarDeclList *vlist;
    char *varp = new char[2], *varP = new char[2], *vardol = new char[2];
    strcpy(varp, "p");
    strcpy(varP, "P");
    strcpy(vardol, "$");
    varp = symbolTable.insertString(varp),
    varP = symbolTable.insertString(varP),
    vardol = symbolTable.insertString(vardol);

    if (options.mode == TREE) {
      // m2l-tree --> tree;
      //              var2 $ where all1 p: (p in $) => ((p^ in $) | (p^=p));
      //              allpos $;
      //              defaultwhere1(p) = p in $;
      //              defaultwhere2(P) = P sub $;

      // defaultwhere2(P) = P sub $;
      exp1 = new UntypedExp_Name(new Name(varP, dummyPos), dummyPos);
      exp2 = new UntypedExp_Name(new Name(vardol, dummyPos), dummyPos);
      exp = new UntypedExp_Sub(exp1, exp2, dummyPos);
      declarations->push_front
	(new Default_Declaration(vVar2, new Name(varP, dummyPos), 
				 exp, dummyPos));
      
      // defaultwhere1(p) = p in $;
      exp1 = new UntypedExp_Name(new Name(varp, dummyPos), dummyPos);
      exp2 = new UntypedExp_Name(new Name(vardol, dummyPos), dummyPos);
      exp = new UntypedExp_In(exp1, exp2, dummyPos);
      declarations->push_front
	(new Default_Declaration(vVar1, new Name(varp, dummyPos), 
				 exp, dummyPos));

      // allpos $;
      declarations->push_front
	(new AllPos_Declaration(new Name(vardol, dummyPos), dummyPos));

      // var2 $ where all1 p: (p in $) => ((p^ in $) | (p^=p));
      exp1 = new UntypedExp_Name(new Name(varp, dummyPos), dummyPos);
      exp1 = new UntypedExp_Up(exp1, dummyPos);
      exp2 = new UntypedExp_Name(new Name(vardol, dummyPos), dummyPos);
      exp = new UntypedExp_In(exp1, exp2, dummyPos);

      exp1 = new UntypedExp_Name(new Name(varp, dummyPos), dummyPos);
      exp1 = new UntypedExp_Up(exp1, dummyPos);
      exp2 = new UntypedExp_Name(new Name(varp, dummyPos), dummyPos);
      exp1 = new UntypedExp_Equal(exp1, exp2, dummyPos);
      numImplicitUp = 2;

      exp = new UntypedExp_Or(exp, exp1, dummyPos);

      exp1 = new UntypedExp_Name(new Name(varp, dummyPos), dummyPos);
      exp2 = new UntypedExp_Name(new Name(vardol, dummyPos), dummyPos);
      exp1 = new UntypedExp_In(exp1, exp2, dummyPos);

      exp = new UntypedExp_Impl(exp1, exp, dummyPos);

      vlist = new VarDeclList(); 
      vlist->push_back(new VarDecl(new Name(varp, dummyPos), NULL, dummyPos));
      exp = new UntypedExp_All1(NULL, vlist, exp, dummyPos);

      vlist = new VarDeclList(); 
      vlist->push_back(new VarDecl(new Name(vardol, dummyPos), exp, dummyPos));
      declarations->push_front
	(new Variable_Declaration(vVar2, NULL, vlist, dummyPos));
    }
    else {
      if (options.alternativeM2LStr) {
	// m2l-str --> linear;
	//             var1 $ where true;
	//             lastpos $;
	//             defaultwhere1(p) = p <= $;
	//             defaultwhere2(P) = P sub {0,...,$};
	
	// defaultwhere2(P) = P sub {0,...,$};
	elist = new UntypedExpList();
	elist->push_back(new UntypedExp_Int(0, dummyPos));
	elist->push_back(new UntypedExp_Interval(dummyPos));
	elist->push_back(new UntypedExp_Name(new Name(vardol, dummyPos), 
					     dummyPos));
	exp1 = new UntypedExp_Name(new Name(varP, dummyPos), dummyPos);
	exp2 = new UntypedExp_Set(elist, dummyPos);
	exp = new UntypedExp_Sub(exp1, exp2, dummyPos);
	declarations->push_front
	  (new Default_Declaration(vVar2, new Name(varP, dummyPos), 
				   exp, dummyPos));
	
	// defaultwhere1(p) = p <= $;
	exp1 = new UntypedExp_Name(new Name(varp, dummyPos), dummyPos);
	exp2 = new UntypedExp_Name(new Name(vardol, dummyPos), dummyPos);
	exp = new UntypedExp_LessEq(exp1, exp2, dummyPos);
	declarations->push_front
	  (new Default_Declaration(vVar1, new Name(varp, dummyPos), 
				   exp, dummyPos));
	
	// lastpos $;
	declarations->push_front
	  (new LastPos_Declaration(new Name(vardol, dummyPos), dummyPos));
        
	// var1 $ where true;
	vlist = new VarDeclList(); 
	vlist->push_back(new VarDecl(new Name(vardol, dummyPos), 
				     new UntypedExp_True(dummyPos), 
				     dummyPos));
	declarations->push_front
	  (new Variable_Declaration(vVar1, NULL, vlist, dummyPos));
      }
      else {
	// m2l-str --> linear;
	//             var2 $ where ~ex1 p where true: p notin $ & p+1 in $;
	//             allpos $;
	//             defaultwhere1(p) = p in $;
	//             defaultwhere2(P) = P sub $;

	// defaultwhere2(P) = P sub $;
	exp1 = new UntypedExp_Name(new Name(varP, dummyPos), dummyPos);
	exp2 = new UntypedExp_Name(new Name(vardol, dummyPos), dummyPos);
	exp = new UntypedExp_Sub(exp1, exp2, dummyPos);
	declarations->push_front
	  (new Default_Declaration(vVar2, new Name(varP, dummyPos), 
				   exp, dummyPos));
	
	// defaultwhere1(p) = p in $;
	exp1 = new UntypedExp_Name(new Name(varp, dummyPos), dummyPos);
	exp2 = new UntypedExp_Name(new Name(vardol, dummyPos), dummyPos);
	exp = new UntypedExp_In(exp1, exp2, dummyPos);
	declarations->push_front
	  (new Default_Declaration(vVar1, new Name(varp, dummyPos), 
				   exp, dummyPos));
	
	// allpos $;
	declarations->push_front
	  (new AllPos_Declaration(new Name(vardol, dummyPos), dummyPos));
	
	// var2 $ where ~ex1 p where true: p notin $ & p+1 in $;
	exp1 = new UntypedExp_Name(new Name(varp, dummyPos), dummyPos);
	exp2 = new UntypedExp_Name(new Name(vardol, dummyPos), dummyPos);
	exp = new UntypedExp_NotIn(exp1, exp2, dummyPos);
	exp1 = new UntypedExp_Name(new Name(varp, dummyPos), dummyPos);
	exp1 = new UntypedExp_Plus(exp1, new ArithExp_Integer(1, dummyPos), 
				   dummyPos);
	exp2 = new UntypedExp_Name(new Name(vardol, dummyPos), dummyPos);
	exp2 = new UntypedExp_In(exp1, exp2, dummyPos);
	exp = new UntypedExp_And(exp, exp2, dummyPos);
	vlist = new VarDeclList(); 
	vlist->push_back(new VarDecl(new Name(varp, dummyPos), 
				     new UntypedExp_True(dummyPos), 
				     dummyPos));
	exp = new UntypedExp_Ex1(NULL, vlist, exp, dummyPos);
	exp = new UntypedExp_Not(exp, dummyPos);
	vlist = new VarDeclList(); 
	vlist->push_back(new VarDecl(new Name(vardol, dummyPos), 
				     exp, dummyPos));
	declarations->push_front
	  (new Variable_Declaration(vVar2, NULL, vlist, dummyPos));
      }
    }
  }
  
  // initialize tree types
  for (unsigned i = 0; i < declarations->size(); i++) {
    Declaration *d = declarations->get(i);
    if (d->kind == dType) {
      symbolTable.insertType(((Type_Declaration *) d)->name);
      numTypes++;
    }
  }
  if (numTypes > 0)
    initTreetypes(numTypes);
  
  // iterate through guide/universe/type declarations
  for (DeclarationList::iterator decl = declarations->begin(); 
       decl != declarations->end(); decl++)
    switch ((*decl)->kind) {
    case dGuide:
    case dUniverse:
    case dType:
      (*decl)->genAST(*monaAST);
    default: ;
    }
  if (numTypes > 0)
    setComponentTypes();

  // make GTA guide
  if (options.mode == TREE)
    makeGTAGuide();

  // make AST of other declarations
  for (DeclarationList::iterator decl = declarations->begin(); 
       decl != declarations->end(); decl++)
    switch ((*decl)->kind) {
    case dGuide:
    case dUniverse:
      break;
    case dType:
      ((Type_Declaration *) (*decl))->checkReachable();
      break;
    default:
      (*decl)->genAST(*monaAST);
    }
  
  return monaAST;
}
