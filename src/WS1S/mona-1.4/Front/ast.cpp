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
#include <string.h>
#include "ast.h"
#include "symboltable.h"
#include "predlib.h"
#include "lib.h"

extern PredicateLib predicateLib;
extern SymbolTable symbolTable;
extern CodeTable *codeTable;
extern AutLib lib;
extern Ident lastPosVar;
extern Ident allPosVar;
extern int numTypes;

//////////  Restriction ///////////////////////////////////////////////////////

static IdentList restrVars; // variables occuring in current restriction
static bool indefault = false; // inside defaultwhere

VarCode
getRestriction(Ident id, SubstCode *subst)
  // make restriction for id using subst
{
  Ident formal;
  VarCode vc;

  // first-order and tree variables need extra restriction
  switch (symbolTable.lookupType(id)) {
  case Varname1:
  case Parname1:
    vc = codeTable->insert(new Code_FirstOrder(id, dummyPos));
    break;
  case VarnameTree:
    vc = codeTable->insert(new Code_WellFormedTree(id, dummyPos));
    break;
  default:
    vc = codeTable->insert(new Code_True(dummyPos));
    break;
  }

  /***
  // add SomeType to non-trees if types in use
  if (numTypes > 0 && symbolTable.lookupType(id) != VarnameTree)
    vc = andList(vc, codeTable->insert(new Code_SomeType(id, dummyPos)));
  ***/  

  if (!restrVars.exists(id)) {
    restrVars.insert(id);
    
    // add user defined or default restriction
    ASTForm *restriction = symbolTable.lookupRestriction(id);
    if (restriction) {
      // add user-defined restriction
      vc = andList(vc, restriction->makeCode(subst));
    }
    else if (!symbolTable.lookupImplicit(id) &&
	     !indefault) { // try default if not implicit variable and not in default
      indefault = true;
      switch (symbolTable.lookupType(id)) {
      case Varname1:
      case Parname1:
	restriction = symbolTable.getDefault1Restriction(&formal);
	break;
      case Varname2:
      case Parname2:
	restriction = symbolTable.getDefault2Restriction(&formal);
	break;
      default: 
	formal = -1;
      } 
      if (restriction && formal != -1) {
	// variable has default restriction, substitute formal parameter
	subst = new SubstCode[2];
	subst[0].formal = formal;
	subst[0].kind = sIdent;
	subst[0].ident = id;
	subst[1].formal = -1;
	vc = andList(vc, restriction->makeCode(subst));
	delete[] subst;
      }
      indefault = false;
    }
    
    restrVars.remove(id);
  }
  return vc;
}

//////////  State Space Shifting //////////////////////////////////////////////

IdentList *
shiftStatespaces(IdentList *univs, int dir)
{
  IdentList *res = NULL;
  if (univs) {
    res = new IdentList;
    for (Ident *id = univs->begin(); id != univs->end(); id++)
      if (symbolTable.lookupType(*id)==Statespacename) {
	unsigned i, n;
	switch (dir) {
	case 0:
	  res->push_back(symbolTable.lookupStatespaceId
			 (guide.muLeft[symbolTable.lookupNumber(*id)]));
	  break;
	case 1:
	  res->push_back(symbolTable.lookupStatespaceId
			 (guide.muRight[symbolTable.lookupNumber(*id)]));
	  break;
	case -1:
	  n = symbolTable.lookupNumber(*id);
	  for (i = 0; i < guide.numSs; i++)
	    if (guide.muLeft[i]==n || guide.muRight[i]==n)
	      res->push_back(symbolTable.lookupStatespaceId(i));
	  break;
	}
      }
      else
	res->push_back(*id);
  }
  return res;
}

//////////  Substitution //////////////////////////////////////////////////////

SubstCode*
lookupSubst(SubstCode *subst, Ident var)
{
  if (subst) {
    unsigned i = 0;
    // search for var in subst list
    while (subst[i].formal != -1) { // array is terminated with .formal=-1
      if (subst[i].formal == var) 
	return &subst[i];
      i++;
    }
  }
  return 0;
}

ASTTermCode*
substitute12UCode(SubstCode *subst, Ident var)
  // substitute var1/var2/tree/univ-Ident with ASTTermCode according to subst 
{
  SubstCode *s = lookupSubst(subst, var);

  if (s) {
    if (s->kind == sTermCode) {
      ASTTermCode *t = s->termCode;
      ASTTermCode *res = new ASTTermCode(t);
      res->code.code->refs++;
      res->code.vars = 
	res->code.vars ? (IdentList *) res->code.vars->copy() : 0;
      return res;
    }
    else { 
      invariant(s->kind == sIdent); // must be sIdent
      var = s->ident; // substitute
    }
  }

  if (symbolTable.lookupType(var) == Univname ||
      symbolTable.lookupType(var) == Statespacename)
    // convert universe name to variable with that universe
    return new ASTTermCode
      (symbolTable.insertFresh(Varname2, new IdentList(var)),
       true, 
       codeTable->insert(new Code_True(dummyPos)));
  else {
    VarCode r = getRestriction(var, subst);
    
    // restrict if not trivial restriction
    if (r.code->kind != cTrue)
      r = codeTable->insert(new Code_Restrict(r, r.code->pos));
  
    return new ASTTermCode(var, false, r);
  }
}

VarCode
substitute0Code(SubstCode *subst, Ident var)
  // substitute var0-Ident with VarCode according to subst 
{
  if (subst) {
    unsigned i = 0;
    while (subst[i].formal != -1) { // array is terminated with .formal=-1
      if (subst[i].formal == var) {
	if (subst[i].kind == sVarCode) {
	  VarCode res = *subst[i].varCode;
	  res.code->refs++;
	  res.vars = res.vars ? (IdentList *) res.vars->copy() : 0;
	  return res;
	}
	else {
	  invariant(subst[i].kind == sIdent); // must be sIdent
	  var = subst[i].ident; // substitute
	  break;
	}
      }
      i++;
    }
  }

  VarCode r = getRestriction(var, subst);
  
  // restrict if not trivial restriction
  if (r.code->kind != cTrue)
    r = codeTable->insert(new Code_Restrict(r, r.code->pos));

  // need to make new VarCode
  return andList(codeTable->insert(new Code_BoolVar(var, dummyPos)), r);
}

IdentList*
substituteUnivs(SubstCode *subst, IdentList *ul)
  // substitute idents in universe list according to subst,
  // inherit universes from variables
  // sort resulting list
{
  if (!ul || ul->empty())
    return NULL;
  IdentList *univs = new IdentList;

  for (Ident *u = ul->begin(); u != ul->end(); u++) {
    unsigned i = 0;
    
    while (subst && subst[i].formal != -1) { 
      // array is terminated with .formal=-1
      if (subst[i].formal == *u) { // found it, do substitution
	IdentList *varunivs = NULL;

	switch (subst[i].kind) {
	case sIdent:
	  switch (symbolTable.lookupType(subst[i].ident)) {
	  case Varname1:
	  case Varname2:
	  case VarnameTree:
	    varunivs = symbolTable.lookupUnivs(subst[i].ident);
	    break;
	  case Univname:
	  case Statespacename:
	    univs->push_back(subst[i].ident); 
	    break;
	  default: // not possible
	    invariant(false);
	  }	    
	  break;
	case sTermCode:
	  varunivs = symbolTable.lookupUnivs(subst[i].termCode->var);
	  break;
	default:  // illegal, caught by type checker
	  invariant(false);
	}
	if (varunivs)
	  univs->insert(varunivs); // (inefficient)
	break;
      }
      i++;
    }
    
    if (!subst || subst[i].formal == -1) { 
      // no substitution of this ident in subst
      switch (symbolTable.lookupType(*u)) {
	
      case Univname:
      case Statespacename:
	univs->push_back(*u); // no inheritance
	break;
	
      case Varname1:
      case Varname2:
      case Parname1:
      case Parname2:
      case VarnameTree:
	// inherit universes from the variable
	{
	  IdentList *varunivs = symbolTable.lookupUnivs(*u);
	  if (varunivs)
	    univs->insert(varunivs); // (inefficient)
	  break;
	}
      default: // not possible 
	invariant(false);
      }
    }
  }
  
  univs->sort();
  return univs;
}
 
int
substCodeLen(SubstCode *sc)
{
  int i = 0;
  while (sc && sc[i].formal!=-1)
    i++;
  return i;
}

//////////  BitList ///////////////////////////////////////////////////////////

BitList::BitList(char *str)
{
  for (; *str; str++)
    if (*str == '0') 
      push_back(Zero);
    else
      push_back(One);
}

//////////  makeCode() ////////////////////////////////////////////////////////

ASTTermCode*
ASTTerm1_Var1::makeCode(SubstCode *subst)
{ 
  return substitute12UCode(subst, n);
}

ASTTermCode*
ASTTerm1_Dot::makeCode(SubstCode *subst)
{
  ASTTermCode *res = t->makeCode(subst);

  BitList::iterator i;
  for (i = bits->begin(); i != bits->end(); i++) {
    Ident tmpVar;

    if ((*i) == Zero) {
      IdentList *univs = shiftStatespaces(symbolTable.lookupUnivs(res->var), 0);
      tmpVar = symbolTable.insertFresh(Varname1, univs);
      
      res->code = 
	project(andList(codeTable->insert
			(new Code_EqDot0(tmpVar, res->var, pos)),
			res->code),
		res, pos);
    }
    else {
      IdentList *univs = shiftStatespaces(symbolTable.lookupUnivs(res->var), 1);
      tmpVar = symbolTable.insertFresh(Varname1, univs);

      res->code = 
	project(andList(codeTable->insert
			(new Code_EqDot1(tmpVar, res->var, pos)),	     
			res->code),
		res, pos);
    }
    
    res->var = tmpVar;
    res->fresh = true;
  }

  return res;
}

ASTTermCode*
ASTTerm1_Up::makeCode(SubstCode *subst)
{
  ASTTermCode *ct = t->makeCode(subst);
 
  IdentList *univs = shiftStatespaces(symbolTable.lookupUnivs(ct->var), -1);
  Ident var = 
    symbolTable.insertFresh(Varname1, univs);

  ASTTermCode *res = new ASTTermCode
    (var, 
     true,
     project(andList(codeTable->insert(new Code_EqUp(var, ct->var, pos)),
		     ct->code),
	     ct, pos));
  
  delete ct;
  return res;
}

ASTTermCode*
ASTTerm1_Root::makeCode(SubstCode *subst)
{
  if (univ == -1) {
    IdentList *univs = symbolTable.allUnivs();
    univ = univs->get(0);
    delete univs;
  }

  IdentList d1(univ);
  IdentList *d2 = substituteUnivs(subst, &d1);
  if (d2->size() != 1) {
    cout << "Error:";
    pos.printsource();
    cout << "\n'root' applied onto multiple universes\n" 
	 << "Execution aborted\n";
    exit(-1);
  }
  univ = d2->get(0);
  if (symbolTable.lookupType(univ)!=Univname) {
    cout << "Error:";
    pos.printsource();
    cout << "\nIllegal argument to 'root'\n"
	 << "Execution aborted\n";
    exit(-1);
  }
  delete d2;

  Ident var = symbolTable.insertFresh(Varname1, new IdentList(univ));
  
  return new ASTTermCode
    (var,
     true,
     andList(codeTable->insert(new Code_EqRoot(var, new IdentList(univ), pos)),
	     codeTable->insert(new Code_FirstOrder(var, pos))));
}
  
int 
ASTTerm1_Int::value() 
{ 
  return n; 
}

ASTTermCode*
ASTTerm1_Int::makeCode(SubstCode *)
{
  if (n < 0) { // difficult to detect during type checking
    cout << "Error:";
    pos.printsource();
    cout << "\nNegative integer encountered\n" << "Execution aborted\n";
    exit(-1);
  }

  Ident var = symbolTable.insertFresh(Varname1);

  return new ASTTermCode
    (var,
     true, 
     codeTable->insert(new Code_EqConst(var, n, pos)));
}
  
ASTTermCode*
ASTTerm1_Plus::makeCode(SubstCode *subst)
{
  ASTTermCode *ct = t->makeCode(subst);

  Ident var = 
    symbolTable.insertFresh(Varname1, copy(symbolTable.lookupUnivs(ct->var)));

  ASTTermCode *res = new ASTTermCode
    (var,
     true,
     project(andList
	     (codeTable->insert(new Code_EqPlus1(var, ct->var, n, pos)),
	      ct->code),
	     ct, pos));
  delete ct;
  return res;
}

ASTTermCode*
ASTTerm1_Minus::makeCode(SubstCode *subst)
{
  ASTTermCode *ct = t->makeCode(subst);

  Ident var = 
    symbolTable.insertFresh(Varname1, copy(symbolTable.lookupUnivs(ct->var)));
  
  ASTTermCode *res = new ASTTermCode
    (var,
     true,
     project(andList(unfold(var, ct->var, n, subst, pos),
		     ct->code),
	     ct, pos));
  
  delete ct;
  return res;
}
  
VarCode
ASTTerm1_Minus::unfold(int v1, int v2, int n, SubstCode *subst, Pos pos)
{
  if (n == 0) 
    return codeTable->insert(new Code_Eq1(v1, v2, pos));
  else {
    int v = symbolTable.insertFresh(Varname2, copy(symbolTable.lookupUnivs(v2)));

    return codeTable->insert
      (new Code_Project
       (v, 
	andList(unfold(v, v2, n-1, subst, pos),
		codeTable->insert(new Code_EqMinus1(v1, v, pos))), 
	pos));
  }
}

ASTTermCode*
ASTTerm1_PlusModulo::makeCode(SubstCode *subst)
{
  ASTTermCode *ct1 = t1->makeCode(subst);
  ASTTermCode *ct2 = t2->makeCode(subst);

  Ident var = symbolTable.insertFresh(Varname1);

  ASTTermCode *res = new ASTTermCode
    (var,
     true,
     project(project(andList(unfold(var, ct1->var, n, ct2->var, 
				    subst, pos),
			     ct1->code,
			     ct2->code),
		     ct1, pos),
	     ct2, pos));

  delete ct1;
  delete ct2;
  return res;
}
  
VarCode
ASTTerm1_PlusModulo::unfold(int v1, int v2, int n, int v3, 
			    SubstCode *subst, Pos pos)
{
  if (n == 0) 
    return codeTable->insert(new Code_Eq1(v1, v2, pos));
  else {
    int v = symbolTable.insertFresh(Varname2, copy(symbolTable.lookupUnivs(v2)));

    return codeTable->insert
      (new Code_Project
       (v, 
	andList(unfold(v, v2, n-1, v3, subst, pos),
		codeTable->insert(new Code_EqPlusModulo(v1, v, v3, pos))), 
	pos));
  }
}

ASTTermCode*
ASTTerm1_MinusModulo::makeCode(SubstCode *subst)
{
  ASTTermCode *ct1 = t1->makeCode(subst);
  ASTTermCode *ct2 = t2->makeCode(subst);

  Ident var = symbolTable.insertFresh(Varname1);

  ASTTermCode *res = new ASTTermCode
    (var,
     true,
     project(project(andList(unfold(var, ct1->var, n, ct2->var, 
				    subst, pos),
			     ct1->code,
			     ct2->code),
		     ct1, pos),
	     ct2, pos));
  
  delete ct1;
  delete ct2;
  return res;
}
  
VarCode
ASTTerm1_MinusModulo::unfold(int v1, int v2, int n, int v3, 
			     SubstCode *subst, Pos pos)
{
  if (n == 0) 
    return codeTable->insert(new Code_Eq1(v1, v2, pos));
  else {
    int v = symbolTable.insertFresh(Varname2, copy(symbolTable.lookupUnivs(v2)));

    return codeTable->insert
      (new Code_Project
       (v, 
	andList(unfold(v, v2, n-1, v3, subst, pos),
		codeTable->insert(new Code_EqMinusModulo(v1, v, v3, pos))), 
	pos));
  }
}

ASTTermCode*
ASTTerm1_Min::makeCode(SubstCode *subst)
{
  ASTTermCode *ct = T->makeCode(subst);

  Ident var = 
    symbolTable.insertFresh(Varname1, copy(symbolTable.lookupUnivs(ct->var)));
  
  ASTTermCode *res = new ASTTermCode
    (var,
     true,
     project(andList
	     (codeTable->insert(new Code_EqMin(var, ct->var, pos)),
	      ct->code),
	     ct, pos));
  
  delete ct;
  return res;
}

ASTTermCode*
ASTTerm1_Max::makeCode(SubstCode *subst)
{
  ASTTermCode *ct = T->makeCode(subst);

  Ident var = 
    symbolTable.insertFresh(Varname1, copy(symbolTable.lookupUnivs(ct->var)));
  
  ASTTermCode *res = new ASTTermCode
    (var,
     true,
     project(andList
	     (codeTable->insert(new Code_EqMax(var, ct->var, pos)),
	      ct->code),
	     ct, pos));

  delete ct;
  return res;
}

ASTTermCode*
ASTTerm1_TreeRoot::makeCode(SubstCode *subst)
{
  ASTTermCode *ct = T->makeCode(subst);
  ASTTermCode *ct2 = T->makeCode(subst);

  // treeroot(T) --> [T] p:  p in T &
  //                         ~ex1 [T] t: t in T & t<p (& firstorder..)
  Ident var = 
    symbolTable.insertFresh(Varname1, copy(symbolTable.lookupUnivs(ct->var)));
  Ident t = 
    symbolTable.insertFresh(Varname1, copy(symbolTable.lookupUnivs(ct->var)));

  VarCode c1 = codeTable->insert(new Code_In(var, ct->var, pos));
  VarCode c2a = project(andList(codeTable->insert(new Code_In(t, ct2->var, pos)),
				ct2->code),
			ct2, pos);
  VarCode c2b = codeTable->insert(new Code_Less1(t, var, pos));
  VarCode c2c = codeTable->insert(new Code_FirstOrder(var, pos));
  c2c = codeTable->insert(new Code_Restrict(c2c, pos));
  VarCode c2d = codeTable->insert(new Code_FirstOrder(t, pos));
  c2d = codeTable->insert(new Code_Restrict(c2d, pos));
  VarCode c2 = codeTable->insert(new Code_And(c2a, c2b, pos));
  c2 = codeTable->insert(new Code_And(c2c, c2, pos));
  c2 = codeTable->insert(new Code_And(c2d, c2, pos));
  c2 = codeTable->insert(new Code_Project(t, c2, pos));
  c2 = codeTable->insert(new Code_Negate(c2, pos));

  ASTTermCode *res = new ASTTermCode
    (var,
     true,
     project(andList
	     (codeTable->insert(new Code_And(c1, c2, pos)),
	      ct->code),
	     ct, pos));

  delete ct;
  delete ct2;
  return res;
}

ASTTermCode*
ASTTerm2_Var2::makeCode(SubstCode *subst)
{
  return substitute12UCode(subst, n);
}

ASTTermCode*
ASTTerm2_VarTree::makeCode(SubstCode *subst)
{
  return substitute12UCode(subst, n);
}

ASTTermCode*
ASTTerm2_Dot::makeCode(SubstCode *subst)
{
  ASTTermCode *res = T->makeCode(subst);

  BitList::iterator i;

  for (i = bits->begin(); i != bits->end(); i++) {
    Ident tmpVar;

    if ((*i) == Zero) {
      IdentList *univs = shiftStatespaces(symbolTable.lookupUnivs(res->var), 0);
      tmpVar = symbolTable.insertFresh(Varname1, univs);
      
      res->code = 
	project(andList
		(codeTable->insert(new Code_EqDot0(tmpVar, res->var, pos)),
		 res->code),
		res, pos);
    }
    else {
      IdentList *univs = shiftStatespaces(symbolTable.lookupUnivs(res->var), 1);
      tmpVar = symbolTable.insertFresh(Varname1, univs);
      
      res->code = 
	project(andList
		(codeTable->insert(new Code_EqDot1(tmpVar, res->var, pos)),
		 res->code),
		res, pos);
    }
    
    res->var = tmpVar;
    res->fresh = true;
  }

  return res;
}

ASTTermCode*
ASTTerm2_Up::makeCode(SubstCode *subst)
{
  ASTTermCode *ct = T->makeCode(subst);
 
  IdentList *univs = shiftStatespaces(symbolTable.lookupUnivs(ct->var), -1);
  Ident var = 
    symbolTable.insertFresh(Varname2, univs);

  ASTTermCode *res = new ASTTermCode
    (var, 
     true,
     project(andList(codeTable->insert(new Code_EqUp(var, ct->var, pos)),
		     ct->code),
	     ct, pos));
  
  delete ct;
  return res;
}
  
ASTTermCode*
ASTTerm2_Empty::makeCode(SubstCode *)
{
  Ident var = symbolTable.insertFresh(Varname2);

  return new ASTTermCode
    (var,
     true,
     codeTable->insert(new Code_EqEmpty(var, pos)));
}

ASTTermCode*
ASTTerm2_Union::makeCode(SubstCode *subst)
{
  ASTTermCode *ct1 = T1->makeCode(subst);
  ASTTermCode *ct2 = T2->makeCode(subst);

  IdentList *univs = ident_union(symbolTable.lookupUnivs(ct1->var),
				 symbolTable.lookupUnivs(ct2->var));

  Ident var = symbolTable.insertFresh(Varname2, univs);
  
  ASTTermCode *res = new ASTTermCode
    (var,
     true,
     project(project(andList(codeTable->insert
			     (new Code_EqUnion(var, ct1->var, ct2->var, pos)),
			     ct1->code,
			     ct2->code),
		     ct1, pos),
	     ct2, pos));
  
  delete ct1;
  delete ct2;
  return res;
}

ASTTermCode*
ASTTerm2_Inter::makeCode(SubstCode *subst)
{
  ASTTermCode *ct1 = T1->makeCode(subst);
  ASTTermCode *ct2 = T2->makeCode(subst);

  IdentList *univs = ident_union(symbolTable.lookupUnivs(ct1->var),
				 symbolTable.lookupUnivs(ct2->var));

  Ident var = symbolTable.insertFresh(Varname2, univs);

  ASTTermCode *res = new ASTTermCode
    (var,
     true,
     project(project(andList(codeTable->insert
			     (new Code_EqInter(var, ct1->var, ct2->var, pos)),
			     ct1->code,
			     ct2->code),
		     ct1, pos),
	     ct2, pos));
  
  delete ct1;
  delete ct2;
  return res;
}

ASTTermCode*
ASTTerm2_Setminus::makeCode(SubstCode *subst)
{
  ASTTermCode *ct1 = T1->makeCode(subst);
  ASTTermCode *ct2 = T2->makeCode(subst);

  IdentList *univs = copy(symbolTable.lookupUnivs(ct1->var));
  
  Ident var = symbolTable.insertFresh(Varname2, univs);

  ASTTermCode *res = new ASTTermCode
    (var,
     true,
     project(project(andList(codeTable->insert
			     (new Code_EqSetMinus(var, ct1->var, ct2->var, pos)),
			     ct1->code,
			     ct2->code),
		     ct1, pos),
	     ct2, pos));
  
  delete ct1;
  delete ct2;
  return res;
}

ASTTermCode*
ASTTerm2_Set::makeCode(SubstCode *subst)
{
  Ident var = symbolTable.insertFresh(Varname2);

  if (elements->empty()) 
    return new ASTTermCode(var,
			    true,
			    codeTable->insert(new Code_EqEmpty(var, pos)));

  ASTTermCode *res = NULL;
  ASTList::iterator i;
  for (i = elements->begin(); i != elements->end(); i++) {
    ASTTermCode *ct;

    if ((*i)->kind == aInterval)
      ct = ((ASTTerm2 *) *i)->makeCode(subst);
    else { // if not an interval, element must be first-order
      ct = ((ASTTerm1 *) *i)->makeCode(subst);

      Ident t = 
	symbolTable.insertFresh(Varname2, copy(symbolTable.lookupUnivs(ct->var)));

      ASTTermCode *ct2 = 
	new ASTTermCode
	(t,
	 true,
	 project(andList(codeTable->insert(new Code_Singleton(t, pos)),
			 codeTable->insert(new Code_In(ct->var, t, pos)),
			 ct->code),
		 ct, pos));
      
      delete ct;
      ct = ct2;
    }

    ASTTermCode *nextres;
    if (res == NULL)
      nextres = ct;
    else {
      IdentList *univs = ident_union(symbolTable.lookupUnivs(ct->var),
				     symbolTable.lookupUnivs(res->var));
      
      Ident var = symbolTable.insertFresh(Varname2, univs);
      
      nextres = 
	new ASTTermCode
	(var,
	 true,
	 project(project(andList(codeTable->insert(new Code_EqUnion(var,
								   res->var,
								   ct->var,
								   pos)),
				 res->code,
				 ct->code),
			 res, pos), 
		 ct, pos));
      delete res;
    }
    res = nextres;
  }

  return res;
}

ASTTermCode*
ASTTerm2_Plus::makeCode(SubstCode *subst)
{
  ASTTermCode *ct = T->makeCode(subst);

  Ident var = 
    symbolTable.insertFresh(Varname2, copy(symbolTable.lookupUnivs(ct->var)));
  
  ASTTermCode *res = new ASTTermCode
    (var,
     true,
     project(andList(unfold(var, ct->var, n, subst, pos), 
		     ct->code),
	     ct, pos));

  delete ct;
  return res;
}

VarCode
ASTTerm2_Plus::unfold(int v1, int v2, int n, SubstCode *subst, Pos pos)
{
  if (n == 0) 
    return codeTable->insert(new Code_Eq2(v1, v2, pos));
  else {
    int v = 
      symbolTable.insertFresh(Varname2, copy(symbolTable.lookupUnivs(v2)));

    return codeTable->insert
      (new Code_Project
       (v, 
	andList(unfold(v, v2, n-1, subst, pos),
		codeTable->insert(new Code_EqPlus2(v1, v, pos))), pos));
  }
}

ASTTermCode*
ASTTerm2_Minus::makeCode(SubstCode *subst)
{
  ASTTermCode *ct = T->makeCode(subst);

  Ident var = 
    symbolTable.insertFresh(Varname2, copy(symbolTable.lookupUnivs(ct->var)));
  
  ASTTermCode *res = new ASTTermCode
    (var,
     true,
     project(andList(unfold(var, ct->var, n, subst, pos), 
		     ct->code),
	     ct, pos));
  
  delete ct;
  return res;
}

VarCode
ASTTerm2_Minus::unfold(int v1, int v2, int n, SubstCode *subst, Pos pos)
{
  if (n == 0) 
    return codeTable->insert(new Code_Eq2(v1, v2, pos));
  else {
    int v = 
      symbolTable.insertFresh(Varname2, copy(symbolTable.lookupUnivs(v2)));

    return codeTable->insert
      (new Code_Project
       (v, 
	andList(unfold(v, v2, n-1, subst, pos),
		codeTable->insert(new Code_EqMinus2(v1, v, pos))), pos));
  }
}

ASTTermCode*
ASTTerm2_Interval::makeCode(SubstCode *subst)
{
  ASTTermCode *ct1 = t1->makeCode(subst);
  ASTTermCode *ct2 = t2->makeCode(subst);
  VarCode code;
  
  IdentList *univs = ident_union(symbolTable.lookupUnivs(ct1->var),
				 symbolTable.lookupUnivs(ct2->var));
  
  Ident var = symbolTable.insertFresh(Varname2, copy(univs));
  Ident v = symbolTable.insertFresh(Varname1, univs);
    
  code = 
    project(project(andList
		    (codeTable->insert(new Code_LessEq1(ct1->var, v, pos)),
		     codeTable->insert(new Code_LessEq1(v, ct2->var, pos)),
		     ct1->code,
		     ct2->code),
		    ct1, pos),
	    ct2, pos);
  
  code = andList(codeTable->insert(new Code_FirstOrder(v, pos)),
		 codeTable->insert
		 (new Code_Negate
		  (codeTable->insert
		   (new Code_Biimpl
		    (codeTable->insert(new Code_In(v, var, pos)),
		     code, pos)), pos)));
  
  ASTTermCode *res = new ASTTermCode
    (var,
     true,
     codeTable->insert(new Code_Negate
		      (codeTable->insert
		       (new Code_Project(v, code, pos)), pos)));
  
  delete ct1;
  delete ct2;
  return res;
}
  
ASTTermCode*
ASTTerm2_PresbConst::makeCode(SubstCode *subst)
{
  Ident var = symbolTable.insertFresh(Varname2, NULL);
  return new ASTTermCode
    (var,
     true,
     codeTable->insert(new Code_EqPresbConst(var, value, pos)));
}

ASTTermCode*
ASTTerm2_Formula::makeCode(SubstCode *subst)
{
  return new ASTTermCode(fresh, true, f->makeCode(subst));
}

VarCode
ASTForm_Var0::makeCode(SubstCode *subst)
{
  return substitute0Code(subst, n);
}

VarCode
ASTForm_True::makeCode(SubstCode *)
{
  return codeTable->insert(new Code_True(pos));
}

VarCode
ASTForm_False::makeCode(SubstCode *)
{
  return codeTable->insert(new Code_False(pos));
}

VarCode
ASTForm_In::makeCode(SubstCode *subst)
{
  ASTTermCode *ct1 = t1->makeCode(subst);
  ASTTermCode *ct2 = T2->makeCode(subst);

  VarCode code = 
    andList(ct1->code,
	    codeTable->insert(new Code_In(ct1->var, ct2->var, pos)),
	    ct2->code);
  
  VarCode res = project(project(code, ct1, pos), ct2, pos);
  
  delete ct1;
  delete ct2;
  return res;
}

VarCode
ASTForm_Notin::makeCode(SubstCode *subst)
{
  ASTTermCode *ct1 = t1->makeCode(subst);
  ASTTermCode *ct2 = T2->makeCode(subst);

  VarCode code = 
    andList(ct1->code,
	    codeTable->insert(new Code_In(ct1->var, ct2->var, pos)),
	    ct2->code);
  
  VarCode res = 
    codeTable->insert(new Code_Negate(project(project(code, 
						     ct1, pos), 
					     ct2, pos),
				     pos));
  
  delete ct1;
  delete ct2;
  return res;
}

VarCode
ASTForm_RootPred::makeCode(SubstCode *subst)
{
  ASTTermCode *ct = t->makeCode(subst);
  
  IdentList *univs = substituteUnivs(subst, ul);
  
  VarCode res = 
    project(andList(codeTable->insert(new Code_EqRoot(ct->var, univs, pos)),
		    ct->code), 
	    ct, pos);
  
  delete ct;
  return res;
}

VarCode
ASTForm_EmptyPred::makeCode(SubstCode *subst)
{
  ASTTermCode *ct = T->makeCode(subst);

  VarCode code = andList(codeTable->insert(new Code_EqEmpty(ct->var, pos)),
			 ct->code);
  
  VarCode res = project(code, ct, pos);

  delete ct;
  return res;
}

VarCode
ASTForm_FirstOrder::makeCode(SubstCode *subst)
{
  ASTTermCode *ct = t->makeCode(subst);

  // nothing to do, makeCode handles restriction
  VarCode res = project(ct->code, ct, pos);

  delete ct;
  return res;
}

VarCode
ASTForm_Sub::makeCode(SubstCode *subst)
{
  ASTTermCode *ct1 = T1->makeCode(subst);
  ASTTermCode *ct2 = T2->makeCode(subst);

  VarCode code = 
    andList(ct1->code,
	    codeTable->insert(new Code_Sub2(ct1->var, ct2->var, pos)),
	    ct2->code);
  
  VarCode res = project(project(code, ct1, pos), ct2, pos);

  delete ct1;
  delete ct2;
  return res;
}

VarCode
ASTForm_Equal1::makeCode(SubstCode *subst)
{
  ASTTermCode *ct1 = t1->makeCode(subst);
  ASTTermCode *ct2 = t2->makeCode(subst);

  VarCode code = 
    andList(ct1->code,
	    codeTable->insert(new Code_Eq1(ct1->var, ct2->var, pos)),
	    ct2->code);
  
  VarCode res = project(project(code, ct1, pos), ct2, pos);

  delete ct1;
  delete ct2;
  return res;
}

VarCode
ASTForm_Equal2::makeCode(SubstCode *subst)
{
  ASTTermCode *ct1 = T1->makeCode(subst);
  ASTTermCode *ct2 = T2->makeCode(subst);

  VarCode code = 
    andList(ct1->code,
	    codeTable->insert(new Code_Eq2(ct1->var, ct2->var, pos)),
	    ct2->code);
  
  VarCode res = project(project(code, ct1, pos), ct2, pos);

  delete ct1;
  delete ct2;
  return res;
}

VarCode
ASTForm_NotEqual1::makeCode(SubstCode *subst)
{
  ASTTermCode *ct1 = t1->makeCode(subst);
  ASTTermCode *ct2 = t2->makeCode(subst);

  VarCode code = 
    andList(ct1->code,
	    codeTable->insert(new Code_Eq1(ct1->var, ct2->var, pos)),
	    ct2->code);
  
  VarCode res = 
    codeTable->insert(new Code_Negate(project(project(code, 
						     ct1, pos), 
					     ct2, pos),
				     pos));
  delete ct1;
  delete ct2;
  return res;
}

VarCode
ASTForm_NotEqual2::makeCode(SubstCode *subst)
{
  ASTTermCode *ct1 = T1->makeCode(subst);
  ASTTermCode *ct2 = T2->makeCode(subst);

  VarCode code = 
    andList(ct1->code,
	    codeTable->insert(new Code_Eq2(ct1->var, ct2->var, pos)),
	    ct2->code);
  
  VarCode res = 
    codeTable->insert(new Code_Negate(project(project(code, 
						     ct1, pos), 
					     ct2, pos),
				     pos));
  delete ct1;
  delete ct2;
  return res;
}

VarCode
ASTForm_Less::makeCode(SubstCode *subst)
{
  ASTTermCode *ct1 = t1->makeCode(subst);
  ASTTermCode *ct2 = t2->makeCode(subst);
  
  VarCode code = 
    andList(ct1->code,
	    codeTable->insert(new Code_Less1(ct1->var, ct2->var, pos)),
	    ct2->code);
  
  VarCode res = project(project(code, ct1, pos), ct2, pos);
  
  delete ct1;
  delete ct2;
  return res;
}

VarCode
ASTForm_LessEq::makeCode(SubstCode *subst)
{
  ASTTermCode *ct1 = t1->makeCode(subst);
  ASTTermCode *ct2 = t2->makeCode(subst);
  
  VarCode code = 
    andList(ct1->code,
	    codeTable->insert(new Code_LessEq1(ct1->var, ct2->var, pos)),
	    ct2->code);
  
  VarCode res = project(project(code, ct1, pos), ct2, pos);

  delete ct1;
  delete ct2;
  return res;
}

VarCode
ASTForm_WellFormedTree::makeCode(SubstCode *subst)
{
  ASTTermCode *ct = T->makeCode(subst);

  VarCode code =
    andList(ct->code,
	    codeTable->insert(new Code_WellFormedTree(ct->var, pos)));
  
  VarCode res = project(code, ct, pos);

  delete ct;
  return res;
}

VarCode
ASTForm_Impl::makeCode(SubstCode *subst)
{
  VarCode c1 = f1->makeCode(subst);
  VarCode c2 = f2->makeCode(subst);

  return codeTable->insert(new Code_Impl(c1, c2, pos));
}

VarCode
ASTForm_Biimpl::makeCode(SubstCode *subst)
{
  VarCode c1 = f1->makeCode(subst);
  VarCode c2 = f2->makeCode(subst);

  return codeTable->insert(new Code_Biimpl(c1, c2, pos));
}

VarCode
ASTForm_And::makeCode(SubstCode *subst)
{
  VarCode c1 = f1->makeCode(subst);
  VarCode c2 = f2->makeCode(subst);

  return codeTable->insert(new Code_And(c1, c2, pos));
}

VarCode
ASTForm_IdLeft::makeCode(SubstCode *subst)
{
  VarCode c1 = f1->makeCode(subst);
  VarCode c2 = f2->makeCode(subst);

  return codeTable->insert(new Code_IdLeft(c1, c2, pos));
}

VarCode
ASTForm_Or::makeCode(SubstCode *subst)
{
  VarCode c1 = f1->makeCode(subst);
  VarCode c2 = f2->makeCode(subst);

  return codeTable->insert(new Code_Or(c1, c2, pos));
}

VarCode
ASTForm_Not::makeCode(SubstCode *subst)
{
  VarCode c = f->makeCode(subst);

  return codeTable->insert(new Code_Negate(c, pos));
}

VarCode
ASTForm_Ex0::makeCode(SubstCode *subst)
{
  return projectList(f->makeCode(subst), vl, pos);
}

VarCode
ASTForm_Ex1::makeCode(SubstCode *subst)
{
  IdentList *univs = substituteUnivs(subst, ul);
  int len = substCodeLen(subst);
  SubstCode *newsubst = new SubstCode[len+vl->size()+1];
  memcpy(newsubst, subst, sizeof(SubstCode)*len);
  IdentList vl2;

  IdentList::iterator i;
  int j;
  for (i = vl->begin(), j = len; i != vl->end(); i++, j++) {
    Ident id = symbolTable.insertFresh(Varname1, copy(univs), false);
    symbolTable.updateRestriction(id, symbolTable.lookupRestriction(*i));
    newsubst[j].formal = *i;
    newsubst[j].kind = sIdent;
    newsubst[j].ident = id;
    vl2.push_back(id);
  } 
  newsubst[j].formal = -1;

  VarCode res = projectList(f->makeCode(newsubst), &vl2, pos);

  for (i = vl2.begin(); i != vl2.end(); i++)
    symbolTable.updateRestriction(*i, NULL);
  delete[] newsubst;
  delete univs;
  return res;
}

VarCode
ASTForm_Ex2::makeCode(SubstCode *subst)
{
  IdentList *univs = substituteUnivs(subst, ul);
  int len = substCodeLen(subst);
  SubstCode *newsubst = new SubstCode[len+vl->size()+1];
  memcpy(newsubst, subst, sizeof(SubstCode)*len);
  IdentList vl2;

  IdentList::iterator i;
  int j;
  for (i = vl->begin(), j = len; i != vl->end(); i++, j++) {
    Ident id = symbolTable.insertFresh(Varname2, copy(univs), false);
    symbolTable.updateRestriction(id, symbolTable.lookupRestriction(*i));
    newsubst[j].formal = *i;
    newsubst[j].kind = sIdent;
    newsubst[j].ident = id;
    vl2.push_back(id);
  } 
  newsubst[j].formal = -1;

  VarCode res = projectList(f->makeCode(newsubst), &vl2, pos);
  for (i = vl2.begin(); i != vl2.end(); i++)
    symbolTable.updateRestriction(*i, NULL);
  delete[] newsubst;
  delete univs;
  return res;
}
  
VarCode
ASTForm_All0::makeCode(SubstCode *subst)
{
  VarCode c = codeTable->insert(new Code_Negate(f->makeCode(subst), pos));

  return codeTable->insert(new Code_Negate(projectList(c, vl, pos), pos));
}

VarCode
ASTForm_All1::makeCode(SubstCode *subst)
{
  IdentList *univs = substituteUnivs(subst, ul);
  int len = substCodeLen(subst);
  SubstCode *newsubst = new SubstCode[len+vl->size()+1];
  memcpy(newsubst, subst, sizeof(SubstCode)*len);
  IdentList vl2;

  IdentList::iterator i;
  int j;
  for (i = vl->begin(), j = len; i != vl->end(); i++, j++) {
    Ident id = symbolTable.insertFresh(Varname1, copy(univs), false);
    symbolTable.updateRestriction(id, symbolTable.lookupRestriction(*i));
    newsubst[j].formal = *i;
    newsubst[j].kind = sIdent;
    newsubst[j].ident = id;
    vl2.push_back(id);
  } 
  newsubst[j].formal = -1;

  VarCode res = codeTable->insert(new Code_Negate(f->makeCode(newsubst), pos));
  res = codeTable->insert(new Code_Negate(projectList(res, &vl2, pos), pos));
  for (i = vl2.begin(); i != vl2.end(); i++)
    symbolTable.updateRestriction(*i, NULL);
  delete[] newsubst;
  delete univs;
  return res;
}

VarCode
ASTForm_All2::makeCode(SubstCode *subst)
{
  IdentList *univs = substituteUnivs(subst, ul);
  int len = substCodeLen(subst);
  SubstCode *newsubst = new SubstCode[len+vl->size()+1];
  memcpy(newsubst, subst, sizeof(SubstCode)*len);
  IdentList vl2;

  IdentList::iterator i;
  int j;
  for (i = vl->begin(), j = len; i != vl->end(); i++, j++) {
    Ident id = symbolTable.insertFresh(Varname2, copy(univs), false);
    symbolTable.updateRestriction(id, symbolTable.lookupRestriction(*i));
    newsubst[j].formal = *i;
    newsubst[j].kind = sIdent;
    newsubst[j].ident = id;
    vl2.push_back(id);
  } 
  newsubst[j].formal = -1;

  VarCode res = codeTable->insert(new Code_Negate(f->makeCode(newsubst), pos));
  res = codeTable->insert(new Code_Negate(projectList(res, &vl2, pos), pos));
  for (i = vl2.begin(); i != vl2.end(); i++)
    symbolTable.updateRestriction(*i, NULL);
  delete[] newsubst;
  delete univs;
  return res;
}

VarCode
ASTForm_Let0::makeCode(SubstCode *subst)
{
  int len = substCodeLen(subst);
  SubstCode *newsubst = new SubstCode[len+defIdents->size()+1];
  memcpy(newsubst, subst, sizeof(SubstCode)*len);
  IdentList idents;
  Deque<VarCode> vcs;

  int k;
  IdentList::iterator i;
  FormList::iterator j;
  for (i = defIdents->begin(), j = defForms->begin(), k = 0; 
       i != defIdents->end(); i++, j++, k++) {
    VarCode vc = (*j)->makeCode(subst);
    Ident id = symbolTable.insertFresh(Varname0, NULL, false);
    symbolTable.updateRestriction(id, symbolTable.lookupRestriction(*i));
    newsubst[len+k].formal = *i;
    newsubst[len+k].kind = sIdent;
    newsubst[len+k].ident = id;
    idents.push_back(id);
    vcs.push_back(vc);
  }
  newsubst[len+k].formal = -1;

  VarCode res = f->makeCode(newsubst);
  Deque<VarCode>::iterator m;
  for (i = idents.begin(), m = vcs.begin(); i != idents.end(); i++, m++) {
    res = codeTable->insert(new Code_Biimpl(*m, codeTable->insert(new Code_BoolVar(*i, pos)), pos));
    res = codeTable->insert(new Code_Project(*i, res, pos));
    symbolTable.updateRestriction(*i, NULL);
  }

  delete[] newsubst;
  return res;
}

VarCode
ASTForm_Let1::makeCode(SubstCode *subst)
{
  int len = substCodeLen(subst);
  SubstCode *newsubst = new SubstCode[len+defIdents->size()+1];
  memcpy(newsubst, subst, sizeof(SubstCode)*len);
  IdentList idents;
  Deque<ASTTermCode *> cts;

  int k;
  IdentList::iterator i;
  Term1List::iterator j;
  for (i = defIdents->begin(), j = defTerms->begin(), k = 0; 
       i != defIdents->end(); i++, j++, k++) {
    ASTTermCode *ct = (*j)->makeCode(subst);
    Ident id = symbolTable.insertFresh(Varname1, copy(symbolTable.lookupUnivs(ct->var)), false);
    symbolTable.updateRestriction(id, symbolTable.lookupRestriction(*i));
    newsubst[len+k].formal = *i;
    newsubst[len+k].kind = sIdent;
    newsubst[len+k].ident = id;
    idents.push_back(id);
    cts.push_back(ct);
  }
  newsubst[len+k].formal = -1;

  VarCode res = f->makeCode(newsubst);
  Deque<ASTTermCode *>::iterator m;
  for (i = idents.begin(), m = cts.begin(); i != idents.end(); i++, m++) {
    res = project(andList(res, codeTable->insert(new Code_Eq1((*m)->var, *i, pos)), 
			  (*m)->code), 
		  *m, pos);
    res = codeTable->insert(new Code_Project(*i, res, pos));
    symbolTable.updateRestriction(*i, NULL);
  }

  delete[] newsubst;
  return res;
}

VarCode
ASTForm_Let2::makeCode(SubstCode *subst)
{
  int len = substCodeLen(subst);
  SubstCode *newsubst = new SubstCode[len+defIdents->size()+1];
  memcpy(newsubst, subst, sizeof(SubstCode)*len);
  IdentList idents;
  Deque<ASTTermCode *> cts;
  
  int k;
  IdentList::iterator i;
  Term2List::iterator j;
  for (i = defIdents->begin(), j = defTerms->begin(), k = 0; 
       i != defIdents->end(); i++, j++, k++) {
    ASTTermCode *ct = (*j)->makeCode(subst);
    Ident id = symbolTable.insertFresh(Varname2, copy(symbolTable.lookupUnivs(ct->var)), false);
    symbolTable.updateRestriction(id, symbolTable.lookupRestriction(*i));
    newsubst[len+k].formal = *i;
    newsubst[len+k].kind = sIdent;
    newsubst[len+k].ident = id;
    idents.push_back(id);
    cts.push_back(ct);
  }
  newsubst[len+k].formal = -1;
  
  VarCode res = f->makeCode(newsubst);
  Deque<ASTTermCode *>::iterator m;
  for (i = idents.begin(), m = cts.begin(); i != idents.end(); i++, m++) {
    res = project(andList(res, codeTable->insert(new Code_Eq2((*m)->var, *i, pos)), 
			  (*m)->code), 
		  *m, pos);
    res = codeTable->insert(new Code_Project(*i, res, pos));
    symbolTable.updateRestriction(*i, NULL);
  }
  
  delete[] newsubst;
  return res;
}

VarCode
ASTForm_Call::makeCode(SubstCode *subst)
{
  PredLibEntry *p = predicateLib.lookup(n);

  if (p->isMacro) { // macro expansion
    // make new substitution list
    SubstCode *newsubst = new SubstCode[p->formals->size() + 1];

    ASTList::iterator actual;
    IdentList::iterator formal;
    Deque<ASTForm*> oldRestrictions;
    Deque<Ident> oldIdents;
    unsigned k;
    IdentList actuals;

    for (actual = args->begin(), formal = p->formals->begin(), k = 0; 
	 actual != args->end(); actual++, formal++, k++) {
      newsubst[k].formal = *formal;

      switch ((*actual)->order) {

      case oTerm1: 
      case oTerm2: 
	{
	  newsubst[k].kind = sTermCode;
	  newsubst[k].termCode = ((ASTTerm *) (*actual))->makeCode(subst);
	  Ident var = newsubst[k].termCode->var;
	  oldRestrictions.push_back(symbolTable.lookupRestriction(var));
	  oldIdents.push_back(var);
	  symbolTable.updateRestriction
	    (var, symbolTable.lookupRestriction(*formal));
	  actuals.push_back(var);
	  break;
	}

      case oForm:
	newsubst[k].kind = sVarCode;
	newsubst[k].varCode = new VarCode;
	*newsubst[k].varCode = ((ASTForm *) (*actual))->makeCode(subst);
	break; // we don't have restrictions on var0's (yet?)

      case oUniv:
	newsubst[k].kind = sTermCode;
	newsubst[k].termCode = 
	  substitute12UCode(subst, ((ASTUniv *) (*actual))->u);
	break;

      default:
	invariant(false);
      }
    }
    newsubst[k].formal = -1; // end of array
 
    // expand macro body using new substitution list
    VarCode res = p->ast->makeCode(newsubst);

    // restore restrictions
    Deque<ASTForm*>::iterator f;
    Deque<Ident>::iterator id;
    for (id = oldIdents.begin(), f = oldRestrictions.begin();
	 id != oldIdents.end(); id++, f++)
      symbolTable.updateRestriction(*id, *f);

    // cleanup newsubst
    for (k = 0; newsubst[k].formal != -1; k++)
      switch (newsubst[k].kind) {
      case sTermCode:
	newsubst[k].termCode->code.remove();
	delete newsubst[k].termCode;
	break;
      case sVarCode:
	newsubst[k].varCode->remove();
	delete newsubst[k].varCode;
	break;
      case sIdent:
	break;
      }
    delete[] newsubst;

    return res;
  }
  else { // predicate call
    IdentList actuals;
    return fold(args->begin(), actuals, subst);
  }
}

VarCode
ASTForm_Call::fold(ASTList::iterator iter,
		   IdentList &actuals, 
		   SubstCode *subst)
{
  VarCode res;
  
  if (iter == args->end()) { // end of recursion
    PredLibEntry *p = predicateLib.lookup(n);

    // make dummy node
    VarCode dummyvc(NULL, NULL);
    Code_PredCall *dummy = 
      new Code_PredCall(n, dummyvc, actuals, p->frees, p->source, pos);

    if (codeTable->exists(*dummy)) {
      // equivalent node already in DAG
      res = codeTable->insert(dummy);
    }
    else { // need to make code
      delete dummy;

      // make new substitution list
      SubstCode *newsubst = new SubstCode[p->formals->size() + 1];
      IdentList::iterator i, j;
      unsigned k;
      for (i = actuals.begin(), j = p->formals->begin(), k = 0;
	   i != actuals.end(); i++, j++, k++) {
	newsubst[k].formal = *j;
	newsubst[k].kind = sIdent;
	newsubst[k].ident = *i;
      }
      newsubst[k].formal = -1; // end of array
      
      // set parameter restrictions
      Deque<ASTForm*> oldRestrictions;
      IdentList::iterator formal, actual;
      for (formal = p->formals->begin(), actual = actuals.begin();
	   formal != p->formals->end(); formal++, actual++) {
	// store the restriction for the actual parameter
	oldRestrictions.push_back(symbolTable.lookupRestriction(*actual));
	// use the restriction for the formal parameter
	symbolTable.updateRestriction
	  (*actual, symbolTable.lookupRestriction(*formal));
      }
      
      // make code for body
      VarCode vc = p->ast->makeCode(newsubst);
      delete[] newsubst;
      
      // insert code
      res = codeTable->insert(new Code_PredCall(n, vc, actuals, p->frees, 
					       p->source, pos));
      
      // restore the restrictions for the actuals
      Deque<ASTForm*>::iterator f;
      for (actual = actuals.begin(), f = oldRestrictions.begin();
	   actual != actuals.end(); actual++, f++)
	symbolTable.updateRestriction(*actual, *f);
    }
  }
  else
    switch ((*iter)->order) {
      
    case oTerm1: 
    case oTerm2: 
      {
	ASTTermCode *ct = ((ASTTerm *) (*iter))->makeCode(subst);
	actuals.push_back(ct->var);
	
	res = project(andList(fold(++iter, actuals, subst), 
			      ct->code),
		      ct, pos);
	delete ct;
	break;
      }
    
    case oForm:
      {
	VarCode vc = ((ASTForm *) (*iter))->makeCode(subst);
	
	if (vc.code->kind == cBoolVar) {
	  Ident id = *(vc.vars->begin());
	  actuals.push_back(id);
	  vc.remove();

	  res = fold(++iter, actuals, subst);
	}
	else {
	  Ident id = symbolTable.insertFresh(Varname0);
	  IdentList idl = IdentList(id);
	  actuals.push_back(id);
	  
	  res = projectList
	    (andList(fold(++iter, actuals, subst), 
		     codeTable->insert(new Code_Biimpl
				      (codeTable->insert(new Code_BoolVar
							(id, pos)),
				       vc, pos))),
	     &idl, pos);
	}
	break;
      }
    
    case oUniv:
      {
	ASTTermCode *ct = substitute12UCode(subst, ((ASTUniv *) (*iter))->u);
	actuals.push_back(ct->var);

	res = project(andList(fold(++iter, actuals, subst), 
			      ct->code),
		      ct, pos);
	delete ct;
	break;
      }
    
    default:
      invariant(false);
    }
  
  return res;
}

VarCode
ASTForm_Import::makeCode(SubstCode *subst)
{
  IdentList::iterator i;
  IdentList *actuals = new IdentList;
  for (i = idents->begin(); i != idents->end(); i++) {
    SubstCode *s = lookupSubst(subst, *i);
    if (s) {
      switch (s->kind) {
      case sTermCode:
	actuals->push_back(s->termCode->var);
	break;
      case sVarCode:
	cout << "Error:";
	pos.printsource();
	cout << "\nIllegal to substitute Boolean expression through macro "
	     << "at import\n" << "Execution aborted\n";
	exit(-1); 
	// (can't do that since macro arguments are code-generated
	// before macro expansion)
      case sIdent:
	actuals->push_back(s->ident);
	break;
      }
    }
    else
      actuals->push_back(*i); // no substitution
  }

  return codeTable->insert(new Code_Import(file, fileVars, actuals, pos));
}

VarCode
ASTForm_Export::makeCode(SubstCode *subst)
{
  VarCode c = f->makeCode(subst);

  return codeTable->insert(new Code_Export(c, file, c.vars, pos));
}

VarCode
ASTForm_Prefix::makeCode(SubstCode *subst)
{
  VarCode c = f->makeCode(subst);

  return codeTable->insert(new Code_Prefix(c, pos));
}

VarCode
ASTForm_Restrict::makeCode(SubstCode *subst)
{
  VarCode c = f->makeCode(subst);

  return codeTable->insert(new Code_Restrict(c, pos));
}

VarCode
ASTForm_InStateSpace1::makeCode(SubstCode *subst)
{
  ASTTermCode *ct = t->makeCode(subst);

  VarCode res = 
    project(andList
	    (codeTable->insert(new Code_InStateSpace(ct->var, 
						     new IdentList(*ss), 
						     pos)),
	     ct->code),
	    ct, pos);

  delete ct;
  return res;
}

VarCode
ASTForm_InStateSpace2::makeCode(SubstCode *subst)
{
  ASTTermCode *ct = T->makeCode(subst);

  VarCode res = 
    project(andList
	    (codeTable->insert(new Code_InStateSpace(ct->var, 
						     new IdentList(*ss), 
						     pos)),
	     ct->code),
	    ct, pos);
  
  delete ct;
  return res;
}

VarCode
ASTForm_SomeType::makeCode(SubstCode *subst)
{
  ASTTermCode *ct = t->makeCode(subst);

  VarCode res = 
    project(andList(codeTable->insert(new Code_SomeType(ct->var, pos)), 
		    ct->code),
	    ct, pos);

  delete ct;
  return res;
}

//////////  Auxiliary functions ///////////////////////////////////////////////

VarCode 
projectList(VarCode vc, IdentList *projList, Pos p)
{
  IdentList::iterator iter;

  if (projList->size() == 0)
    return vc;

  iter = projList->end();
  iter--;

  do
    vc = codeTable->insert(new Code_Project(*iter, vc, p));
  while (iter-- != projList->begin());

  return vc;
}

VarCode 
andList(VarCode vc1, VarCode vc2)
{
  if (vc1.code->kind == cTrue) {
    vc1.remove();
    return vc2;
  }
  else if (vc2.code->kind == cTrue) {
    vc2.remove();
    return vc1;
  }
  return codeTable->insert(new Code_And(vc1, vc2, vc1.code->pos));
}

VarCode 
andList(VarCode vc1, VarCode vc2, VarCode vc3)
{
  return andList(vc1, andList(vc2, vc3));
}

VarCode 
andList(VarCode vc1, VarCode vc2, VarCode vc3, VarCode vc4)
{
  return andList(vc1, andList(vc2, vc3, vc4));
}

VarCode
project(VarCode vc, ASTTermCode *t, Pos p)
{
  if (t->fresh)
    return codeTable->insert(new Code_Project(t->var, vc, p));
  else
    return vc;
}
