//
// ast.cpp
//
// MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
// 
// Reproduction of all or part of this software is permitted for
// educational or research use on condition that this copyright notice is
// included in any copy. This software comes with no warranty of any
// kind. In no event will BRICS be liable for any damages resulting from
// use of this software.

#include <stdlib.h>
#include <string.h>
#include "ast.h"
#include "symboltable.h"
#include "predlib.h"
#include "offsets.h"
#include "env.h"
#include "lib.h"

extern Environment environment; 
extern PredicateLib predicateLib;
extern SymbolTable symbolTable;
extern CodeTable codeTable;
extern Offsets offsets;
extern AutLib lib;
extern Ident lastPosVar;

VarCode getRestriction(Ident id, SubstCode *subst);

VarCode project(VarCode vc, ASTTermCode *t, Pos p);
VarCode projectList(VarCode vc, IdentList *projList, Pos p);

VarCode andList(VarCode vc1, VarCode vc2);
VarCode andList(VarCode vc1, VarCode vc2, VarCode vc3);
VarCode andList(VarCode vc1, VarCode vc2, VarCode vc3, VarCode vc4);

///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//       Restriction                                                         //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

static IdentList restrVars; // variables occuring in current restriction

VarCode
getRestriction(Ident id, SubstCode *subst)
  // make restriction for id using subst
{
  Ident formal;
  VarCode vc;

  // first-order variables need extra restriction
  switch (symbolTable.lookupType(id)) {
  case Varname1:
  case Parname1:
    vc = codeTable.insert(new Code_FirstOrder(id, dummyPos));
    break;
  default:
    vc = codeTable.insert(new Code_True(dummyPos));
    break;
  }

  if (!restrVars.exists(id)) {
    restrVars.insert(id);

    // add user defined or default restriction
    ASTForm *restriction = symbolTable.lookupRestriction(id);
    if (restriction)
      // add user-defined restriction
      vc = andList(vc, restriction->makeCode(subst));
    else { // try default
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
    }

    restrVars.remove(id);
  }
  
  // restrict if not trivial restriction
  if (vc.code->kind != cTrue)
    vc = codeTable.insert(new Code_Restrict(vc, vc.code->pos));
  
  return vc;
}

///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//       Substitution                                                        //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

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
  // substitute var1/var2/univ-Ident with ASTTermCode according to subst 
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

  if (symbolTable.lookupType(var) == Univname)
    // convert universe name to variable with that universe
    return new ASTTermCode(symbolTable.insertFresh(Varname2, new IdentList(var)),
			   true, 
			   codeTable.insert(new Code_True(dummyPos)));
  else
    return new ASTTermCode(var, false, getRestriction(var, subst));
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

  // need to make new VarCode
  return codeTable.insert(new Code_BoolVar(var, dummyPos));
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
	    varunivs = symbolTable.lookupUnivs(subst[i].ident);
	    break;
	  case Univname:
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
	univs->push_back(*u); // no inheritance
	break;
	
      case Varname1:
      case Varname2:
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
 
///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//       AST                                                                 //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

void 
ASTList::dump()
{
  iterator i;
  for (i = begin(); i != end(); i++) {
    cout << ","; (*i)->dump();
  }
}

void 
ASTUniv::dump()
{
  cout << symbolTable.lookupSymbol(u);
}

void 
ASTTerm1_T::freeVars(IdentList *free, IdentList *bound)
{
  T->freeVars(free, bound);
}

void 
ASTTerm1_tn::freeVars(IdentList *free, IdentList *bound)
{
  t->freeVars(free, bound);
}
  
void 
ASTTerm1_tnt::freeVars(IdentList *free, IdentList *bound)
{
  t1->freeVars(free, bound);
  t2->freeVars(free, bound);
}
  
void 
ASTTerm2_TT::freeVars(IdentList *free, IdentList *bound)
{
  T1->freeVars(free, bound);
  T2->freeVars(free, bound);
}

void 
ASTTerm2_Tn::freeVars(IdentList *free, IdentList *bound)
{
  T->freeVars(free, bound);
}

void 
ASTForm_tT::freeVars(IdentList *free, IdentList *bound)
{
  t1->freeVars(free, bound);
  T2->freeVars(free, bound);
}
  
void 
ASTForm_T::freeVars(IdentList *free, IdentList *bound)
{
  T->freeVars(free, bound);
}
  
void 
ASTForm_TT::freeVars(IdentList *free, IdentList *bound)
{
  T1->freeVars(free, bound);
  T2->freeVars(free, bound);
}
  
void 
ASTForm_tt::freeVars(IdentList *free, IdentList *bound)
{
  t1->freeVars(free, bound);
  t2->freeVars(free, bound);
}
  
void 
ASTForm_nt::freeVars(IdentList *free, IdentList *bound)
{
  t->freeVars(free, bound);
}

void 
ASTForm_nT::freeVars(IdentList *free, IdentList *bound)
{
  T->freeVars(free, bound);
}

void 
ASTForm_ff::freeVars(IdentList *free, IdentList *bound)
{
  f1->freeVars(free, bound);
  f2->freeVars(free, bound);
}

void 
ASTForm_vf::freeVars(IdentList *free, IdentList *bound)
{
  bound->insert(vl);
  f->freeVars(free, bound);
}

void 
ASTForm_uvf::freeVars(IdentList *free, IdentList *bound)
{
  bound->insert(vl);
  f->freeVars(free, bound);
}

///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//       ASTTerm1                                                            //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

//////////  ASTTerm1_Var1 /////////////////////////////////////////////////////

void 
ASTTerm1_Var1::freeVars(IdentList *free, IdentList *bound)
{
  if (free->exists(n) || bound->exists(n))
    return; // already been here

  if (!bound->exists(n))
    free->insert(n);
  
  // find free variables of restriction
  ASTForm *restriction = symbolTable.lookupRestriction(n);
  if (restriction)
    restriction->freeVars(free, bound);
  else {
    Ident formal;
    restriction = symbolTable.getDefault1Restriction(&formal);
    if (restriction) {
      bound->insert(formal); // bind the default-restriction formal
      restriction->freeVars(free, bound);
    }
  }
}
  
ASTTermCode*
ASTTerm1_Var1::makeCode(SubstCode *subst)
{ 
  return substitute12UCode(subst, n);
}

void 
ASTTerm1_Var1::dump()
{
  cout << "Var1 " << symbolTable.lookupSymbol(n);
}
  
//////////  ASTTerm1_Dot //////////////////////////////////////////////////////

void 
ASTTerm1_Dot::freeVars(IdentList *free, IdentList *bound)
{
  t->freeVars(free, bound);
}

ASTTermCode*
ASTTerm1_Dot::makeCode(SubstCode *subst)
{
  ASTTermCode *res = t->makeCode(subst);

  BitList::iterator i;
  for (i = bits->begin(); i != bits->end(); i++) {
    Ident tmpVar = 
      symbolTable.insertFresh(Varname1, copy(symbolTable.lookupUnivs(res->var)));

    if ((*i) == Zero)
      res->code = 
	project(andList(codeTable.insert
			(new Code_EqDot0(tmpVar, res->var, pos)),
			res->code),
		res, pos);
    else 
      res->code = 
	project(andList(codeTable.insert
			(new Code_EqDot1(tmpVar, res->var, pos)),	     
			res->code),
		res, pos);
    
    res->var = tmpVar;
    res->fresh = true;
  }

  return res;
}

void 
ASTTerm1_Dot::dump()
{
  cout << "Dot("; t->dump(); cout << ",";
  BitList::iterator i;
  for (i = bits->begin(); i != bits->end(); i++)
    if (*i == Zero)
      cout << "0";
    else
      cout << "1";
  cout << ")";
}
    
//////////  ASTTerm1_Up ///////////////////////////////////////////////////////

void 
ASTTerm1_Up::freeVars(IdentList *free, IdentList *bound)
{
  t->freeVars(free, bound);
}

ASTTermCode*
ASTTerm1_Up::makeCode(SubstCode *subst)
{
  ASTTermCode *ct = t->makeCode(subst);
 
  Ident var = 
    symbolTable.insertFresh(Varname1, copy(symbolTable.lookupUnivs(ct->var)));

  ASTTermCode *res = new ASTTermCode
    (var, 
     true,
     project(andList(codeTable.insert
		     (new Code_EqUp(var, ct->var, pos)),
		     ct->code),
	     ct, pos));
  
  delete ct;
  return res;
}
  
void 
ASTTerm1_Up::dump()
{
  cout << "Up("; t->dump(); cout << ")";
}
    
//////////  ASTTerm1_Root /////////////////////////////////////////////////////

ASTTermCode*
ASTTerm1_Root::makeCode(SubstCode *subst)
{
  if (univ == -1)
    univ = (symbolTable.allUnivs())->get(0);

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
  delete d2;

  Ident var = symbolTable.insertFresh(Varname1, new IdentList(univ));
  
  return new ASTTermCode
    (var,
     true,
     codeTable.insert(new Code_EqRoot(var, new IdentList(univ), pos)));
}

void
ASTTerm1_Root::dump()
{
  cout << "Root(" << symbolTable.lookupSymbol(univ) << ")";
}

//////////  ASTTerm1_Int //////////////////////////////////////////////////////
  
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
     codeTable.insert(new Code_EqConst(var, n, pos)));
}
  
void 
ASTTerm1_Int::dump()
{
  cout << "Int " << n;
}
    
//////////  ASTTerm1_Plus /////////////////////////////////////////////////////

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
	     (codeTable.insert(new Code_EqPlus1(var, ct->var, n, pos)),
	      ct->code),
	     ct, pos));
  delete ct;
  return res;
}

void 
ASTTerm1_Plus::dump()
{
  cout << "Plus1("; t->dump(); cout << "," << n << ")";
}
    
//////////  ASTTerm1_Minus ////////////////////////////////////////////////////

ASTTermCode*
ASTTerm1_Minus::makeCode(SubstCode *subst)
{
  ASTTermCode *ct = t->makeCode(subst);

  Ident var = 
    symbolTable.insertFresh(Varname1, copy(symbolTable.lookupUnivs(ct->var)));
  
  ASTTermCode *res = new ASTTermCode
    (var,
     true,
     project(andList
	     (codeTable.insert(new Code_EqMinus1(var, ct->var, n, pos)),
	      ct->code),
	     ct, pos));
  
  delete ct;
  return res;
}
  
void 
ASTTerm1_Minus::dump()
{
  cout << "Minus1("; t->dump(); cout << "," << n << ")";
}
    
//////////  ASTTerm1_PlusModulo ///////////////////////////////////////////////

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
    return codeTable.insert(new Code_Eq1(v1, v2, pos));
  else {
    int v = symbolTable.insertFresh(Varname2, copy(symbolTable.lookupUnivs(v2)));

    return codeTable.insert
      (new Code_Project
       (v, 
	andList(unfold(v, v2, n-1, v3, subst, pos),
		codeTable.insert(new Code_EqPlusModulo(v1, v, v3, pos))), 
	pos));
  }
}

void 
ASTTerm1_PlusModulo::dump()
{
  cout << "PlusModulo1("; t1->dump(); cout << ","  << n << ",";
  t2->dump(); cout << ")";
}
    
//////////  ASTTerm1_MinusModulo //////////////////////////////////////////////

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
    return codeTable.insert(new Code_Eq1(v1, v2, pos));
  else {
    int v = symbolTable.insertFresh(Varname2, copy(symbolTable.lookupUnivs(v2)));

    return codeTable.insert
      (new Code_Project
       (v, 
	andList(unfold(v, v2, n-1, v3, subst, pos),
		codeTable.insert(new Code_EqMinusModulo(v1, v, v3, pos))), 
	pos));
  }
}

void 
ASTTerm1_MinusModulo::dump()
{
  cout << "MinusModulo1("; t1->dump(); cout << "," << n << ",";
  t2->dump(); cout << ")";
}
    
//////////  ASTTerm1_Min //////////////////////////////////////////////////////

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
	     (codeTable.insert(new Code_EqMin(var, ct->var, pos)),
	      ct->code),
	     ct, pos));
  
  delete ct;
  return res;
}

void 
ASTTerm1_Min::dump()
{
  cout << "Min("; T->dump(); cout << ")";
}
    
//////////  ASTTerm1_Max //////////////////////////////////////////////////////

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
	     (codeTable.insert(new Code_EqMax(var, ct->var, pos)),
	      ct->code),
	     ct, pos));

  delete ct;
  return res;
}

void 
ASTTerm1_Max::dump()
{
  cout << "Max("; T->dump(); cout << ")";
}
    
///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//       ASTTerm2                                                            //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

//////////  ASTTerm2_Var2 /////////////////////////////////////////////////////

void 
ASTTerm2_Var2::freeVars(IdentList *free, IdentList *bound)
{
  if (free->exists(n) || bound->exists(n))
    return; // already been here

  if (!bound->exists(n))
    free->insert(n);
  
  // find free variables of restriction
  ASTForm *restriction = symbolTable.lookupRestriction(n);
  if (restriction)
    restriction->freeVars(free, bound);
  else {
    Ident formal;
    restriction = symbolTable.getDefault2Restriction(&formal);
    if (restriction) {
      bound->insert(formal); // bind the default-restriction formal
      restriction->freeVars(free, bound);
    }
  }
}

ASTTermCode*
ASTTerm2_Var2::makeCode(SubstCode *subst)
{
  return substitute12UCode(subst, n);
}

void 
ASTTerm2_Var2::dump()
{
  cout << "Var2 " << symbolTable.lookupSymbol(n);
}
    
//////////  ASTTerm2_Dot //////////////////////////////////////////////////////

void 
ASTTerm2_Dot::freeVars(IdentList *free, IdentList *bound)
{
  T->freeVars(free, bound);
}

ASTTermCode*
ASTTerm2_Dot::makeCode(SubstCode *subst)
{
  ASTTermCode *res = T->makeCode(subst);

  BitList::iterator i;

  for (i = bits->begin(); i != bits->end(); i++) {
    Ident tmpVar = 
      symbolTable.insertFresh(Varname2, copy(symbolTable.lookupUnivs(res->var)));

    if ((*i) == Zero)
      res->code = 
	project(andList
		(codeTable.insert(new Code_EqDot0(tmpVar, res->var, pos)),
		 res->code),
		res, pos);
    else 
      res->code = 
	project(andList
		(codeTable.insert(new Code_EqDot1(tmpVar, res->var, pos)),
		 res->code),
		res, pos);
    
    res->var = tmpVar;
    res->fresh = true;
  }

  return res;
}

void 
ASTTerm2_Dot::dump()
{
  cout << "Dot("; T->dump(); cout << ",";
  BitList::iterator i;
  for (i = bits->begin(); i != bits->end(); i++)
    if (*i == Zero)
      cout << "0";
    else
      cout << "1";
  cout << ")";
}
    
//////////  ASTTerm2_Up ///////////////////////////////////////////////////////

void 
ASTTerm2_Up::freeVars(IdentList *free, IdentList *bound)
{
  T->freeVars(free, bound);
}

ASTTermCode*
ASTTerm2_Up::makeCode(SubstCode *subst)
{
  ASTTermCode *ct = T->makeCode(subst);
 
  Ident var = 
    symbolTable.insertFresh(Varname2, copy(symbolTable.lookupUnivs(ct->var)));

  ASTTermCode *res = new ASTTermCode
    (var, 
     true,
     project(andList(codeTable.insert(new Code_EqUp(var, ct->var, pos)),
		     ct->code),
	     ct, pos));
  
  delete ct;
  return res;
}
  
void 
ASTTerm2_Up::dump()
{
  cout << "Up("; T->dump(); cout << ")";
}
    
//////////  ASTTerm2_Empty ////////////////////////////////////////////////////

ASTTermCode*
ASTTerm2_Empty::makeCode(SubstCode *)
{
  Ident var = symbolTable.insertFresh(Varname2);

  return new ASTTermCode
    (var,
     true,
     codeTable.insert(new Code_EqEmpty(var, pos)));
}

void 
ASTTerm2_Empty::dump()
{
  cout << "Empty";
}
    
//////////  ASTTerm2_Union ////////////////////////////////////////////////////

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
     project(project(andList(codeTable.insert
			     (new Code_EqUnion(var, ct1->var, ct2->var, pos)),
			     ct1->code,
			     ct2->code),
		     ct1, pos),
	     ct2, pos));
  
  delete ct1;
  delete ct2;
  return res;
}

void 
ASTTerm2_Union::dump()
{
  cout << "Union("; T1->dump(); cout << ","; T2->dump(); cout << ")";
}
    
//////////  ASTTerm2_Inter ////////////////////////////////////////////////////

ASTTermCode*
ASTTerm2_Inter::makeCode(SubstCode *subst)
{
  ASTTermCode *ct1 = T1->makeCode(subst);
  ASTTermCode *ct2 = T2->makeCode(subst);

  IdentList *univs = intersection(symbolTable.lookupUnivs(ct1->var),
				  symbolTable.lookupUnivs(ct2->var));

  Ident var = symbolTable.insertFresh(Varname2, univs);

  ASTTermCode *res = new ASTTermCode
    (var,
     true,
     project(project(andList(codeTable.insert
			     (new Code_EqInter(var, ct1->var, ct2->var, pos)),
			     ct1->code,
			     ct2->code),
		     ct1, pos),
	     ct2, pos));
  
  delete ct1;
  delete ct2;
  return res;
}

void 
ASTTerm2_Inter::dump()
{
  cout << "Inter("; T1->dump(); cout << ","; T2->dump(); cout << ")";
}
    
//////////  ASTTerm2_Setminus /////////////////////////////////////////////////

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
     project(project(andList(codeTable.insert
			     (new Code_EqSetMinus(var, ct1->var, ct2->var, pos)),
			     ct1->code,
			     ct2->code),
		     ct1, pos),
	     ct2, pos));
  
  delete ct1;
  delete ct2;
  return res;
}

void 
ASTTerm2_Setminus::dump()
{
  cout << "Setminus("; T1->dump(); cout << ","; T2->dump(); cout << ")";
}
    
//////////  ASTTerm2_Set //////////////////////////////////////////////////////

void 
ASTTerm2_Set::freeVars(IdentList *free, IdentList *bound)
{
  ASTList::iterator i;
  for (i = elements->begin(); i != elements->end(); i++)
    (*i)->freeVars(free, bound);
}
  
ASTTermCode*
ASTTerm2_Set::makeCode(SubstCode *subst)
{
  Ident var = symbolTable.insertFresh(Varname2);

  if (elements->empty()) 
    return new ASTTermCode(var,
			    true,
			    codeTable.insert(new Code_EqEmpty(var, pos)));

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
	 project(andList(codeTable.insert(new Code_Singleton(t, pos)),
			 codeTable.insert(new Code_In(ct->var, t, pos)),
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
	 project(project(andList(codeTable.insert(new Code_EqUnion(var,
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

void 
ASTTerm2_Set::dump()
{
  ASTList::iterator i;
  cout << "Set(";
  for (i = elements->begin(); i != elements->end();) {
    (*i)->dump();
    if (++i != elements->end()) 
      cout << ",";
  }
  cout << ")";
}
    
//////////  ASTTerm2_Plus ////////////////////////////////////////////////////
  
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
    return codeTable.insert(new Code_Eq2(v1, v2, pos));
  else {
    int v = 
      symbolTable.insertFresh(Varname2, copy(symbolTable.lookupUnivs(v2)));

    return codeTable.insert
      (new Code_Project
       (v, 
	andList(unfold(v, v2, n-1, subst, pos),
		codeTable.insert(new Code_EqPlus2(v1, v, pos))), pos));
  }
}

void 
ASTTerm2_Plus::dump()
{
  cout << "Plus2("; T->dump(); cout << "," << n << ")";
}
    
//////////  ASTTerm2_Minus ///////////////////////////////////////////////////
  
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
    return codeTable.insert(new Code_Eq2(v1, v2, pos));
  else {
    int v = 
      symbolTable.insertFresh(Varname2, copy(symbolTable.lookupUnivs(v2)));

    return codeTable.insert
      (new Code_Project
       (v, 
	andList(unfold(v, v2, n-1, subst, pos),
		codeTable.insert(new Code_EqMinus2(v1, v, pos))), pos));
  }
}

void 
ASTTerm2_Minus::dump()
{
  cout << "Minus2("; T->dump(); cout << "," << n << ")";
}
    
//////////  ASTTerm2_Interval /////////////////////////////////////////////////

void 
ASTTerm2_Interval::freeVars(IdentList *free, IdentList *bound)
{
  t1->freeVars(free, bound);
  t2->freeVars(free, bound);
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
		    (codeTable.insert(new Code_LessEq1(ct1->var, v, pos)),
		     codeTable.insert(new Code_LessEq1(v, ct2->var, pos)),
		     ct1->code,
		     ct2->code),
		    ct1, pos),
	    ct2, pos);
  
  code = andList(codeTable.insert(new Code_FirstOrder(v, pos)),
		 codeTable.insert
		 (new Code_Negate
		  (codeTable.insert
		   (new Code_Biimpl
		    (codeTable.insert(new Code_In(v, var, pos)),
		     code, pos)), pos)));
  
  ASTTermCode *res = new ASTTermCode
    (var,
     true,
     codeTable.insert(new Code_Negate
		      (codeTable.insert
		       (new Code_Project(v, code, pos)), pos)));
  
  delete ct1;
  delete ct2;
  return res;
}
  
void 
ASTTerm2_Interval::dump()
{
  cout << "Interval("; t1->dump(); cout << ","; t2->dump(); cout << ")";
}
    
//////////  ASTTerm2_PresbConst ///////////////////////////////////////////////

ASTTermCode*
ASTTerm2_PresbConst::makeCode(SubstCode *subst)
{
  Ident var = symbolTable.insertFresh(Varname2, NULL);
  return new ASTTermCode
    (var,
     true,
     codeTable.insert(new Code_EqPresbConst(var, value, pos)));
}

void 
ASTTerm2_PresbConst::dump()
{
  cout << "PresbConst(" << value << ")"; 
}

///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//       ASTForm                                                             //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

//////////  ASTForm_Var0 //////////////////////////////////////////////////////

void 
ASTForm_Var0::freeVars(IdentList *free, IdentList *bound)
{
  if (!bound->exists(n))
    free->insert(n);
}

VarCode
ASTForm_Var0::makeCode(SubstCode *subst)
{
  return substitute0Code(subst, n);
}

void 
ASTForm_Var0::dump()
{
  cout << "Var0 " << symbolTable.lookupSymbol(n);
}
    
//////////  AstForm_True //////////////////////////////////////////////////////

VarCode
ASTForm_True::makeCode(SubstCode *)
{
  return codeTable.insert(new Code_True(pos));
}

void 
ASTForm_True::dump()
{
  cout << "True";
}
    
//////////  ASTForm_False /////////////////////////////////////////////////////

VarCode
ASTForm_False::makeCode(SubstCode *)
{
  return codeTable.insert(new Code_False(pos));
}

void 
ASTForm_False::dump()
{
  cout << "False";
}
    
//////////  ASTForm_In ////////////////////////////////////////////////////////

VarCode
ASTForm_In::makeCode(SubstCode *subst)
{
  ASTTermCode *ct1 = t1->makeCode(subst);
  ASTTermCode *ct2 = T2->makeCode(subst);

  VarCode code = 
    andList(ct1->code,
	    codeTable.insert(new Code_In(ct1->var, ct2->var, pos)),
	    ct2->code);
  
  VarCode res = project(project(code, ct1, pos), ct2, pos);
  
  delete ct1;
  delete ct2;
  return res;
}

void 
ASTForm_In::dump()
{
  cout << "In("; t1->dump(); cout << ","; T2->dump(); cout << ")";
}
    
//////////  ASTForm_Notin /////////////////////////////////////////////////////

VarCode
ASTForm_Notin::makeCode(SubstCode *subst)
{
  ASTTermCode *ct1 = t1->makeCode(subst);
  ASTTermCode *ct2 = T2->makeCode(subst);

  VarCode code = 
    andList(ct1->code,
	    codeTable.insert(new Code_In(ct1->var, ct2->var, pos)),
	    ct2->code);
  
  VarCode res = 
    codeTable.insert(new Code_Negate(project(project(code, 
						     ct1, pos), 
					     ct2, pos),
				     pos));
  
  delete ct1;
  delete ct2;
  return res;
}

void 
ASTForm_Notin::dump()
{
  cout << "Notin("; t1->dump(); cout << ","; T2->dump(); cout << ")";
}
    
//////////  ASTForm_RootPred //////////////////////////////////////////////////

void 
ASTForm_RootPred::freeVars(IdentList *free, IdentList *bound)
{
  t->freeVars(free, bound);
}

VarCode
ASTForm_RootPred::makeCode(SubstCode *subst)
{
  ASTTermCode *ct = t->makeCode(subst);
  
  IdentList *univs = substituteUnivs(subst, ul);
  
  VarCode res = 
    project(andList(codeTable.insert(new Code_EqRoot(ct->var, univs, pos)),
		    ct->code), 
	    ct, pos);
  
  delete ct;
  return res;
}

void
ASTForm_RootPred::dump()
{
  cout << "Root("; t->dump(); cout << ",["; ul->dump(); cout << ")"; 
}

//////////  ASTForm_EmptyPred /////////////////////////////////////////////////

VarCode
ASTForm_EmptyPred::makeCode(SubstCode *subst)
{
  ASTTermCode *ct = T->makeCode(subst);

  VarCode code = andList(codeTable.insert(new Code_EqEmpty(ct->var, pos)),
			 ct->code);
  
  VarCode res = project(code, ct, pos);

  delete ct;
  return res;
}

void 
ASTForm_EmptyPred::dump()
{
  cout << "EmptyPred("; T->dump(); cout << ")";
}
    
//////////  ASTForm_FirstOrder ////////////////////////////////////////////////

void 
ASTForm_FirstOrder::freeVars(IdentList *free, IdentList *bound)
{
  t->freeVars(free, bound);
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

void 
ASTForm_FirstOrder::dump()
{
  cout << "FirstOrder("; t->dump(); cout << ")";
}
    
//////////  ASTForm_Sub ///////////////////////////////////////////////////////

VarCode
ASTForm_Sub::makeCode(SubstCode *subst)
{
  ASTTermCode *ct1 = T1->makeCode(subst);
  ASTTermCode *ct2 = T2->makeCode(subst);

  VarCode code = 
    andList(ct1->code,
	    codeTable.insert(new Code_Sub2(ct1->var, ct2->var, pos)),
	    ct2->code);
  
  VarCode res = project(project(code, ct1, pos), ct2, pos);

  delete ct1;
  delete ct2;
  return res;
}

void 
ASTForm_Sub::dump()
{
  cout << "Sub("; T1->dump(); cout << ","; T2->dump(); cout << ")";
}
    
//////////  ASTForm_Equal1 ////////////////////////////////////////////////////

VarCode
ASTForm_Equal1::makeCode(SubstCode *subst)
{
  ASTTermCode *ct1 = t1->makeCode(subst);
  ASTTermCode *ct2 = t2->makeCode(subst);

  VarCode code = 
    andList(ct1->code,
	    codeTable.insert(new Code_Eq1(ct1->var, ct2->var, pos)),
	    ct2->code);
  
  VarCode res = project(project(code, ct1, pos), ct2, pos);

  delete ct1;
  delete ct2;
  return res;
}

void 
ASTForm_Equal1::dump()
{
  cout << "Equal1("; t1->dump(); cout << ","; t2->dump(); cout << ")";
}
    
//////////  ASTForm_Equal2 ////////////////////////////////////////////////////

VarCode
ASTForm_Equal2::makeCode(SubstCode *subst)
{
  ASTTermCode *ct1 = T1->makeCode(subst);
  ASTTermCode *ct2 = T2->makeCode(subst);

  VarCode code = 
    andList(ct1->code,
	    codeTable.insert(new Code_Eq2(ct1->var, ct2->var, pos)),
	    ct2->code);
  
  VarCode res = project(project(code, ct1, pos), ct2, pos);

  delete ct1;
  delete ct2;
  return res;
}

void 
ASTForm_Equal2::dump()
{
  cout << "Equal2("; T1->dump(); cout << ","; T2->dump(); cout << ")";
}
    
//////////  ASTForm_NotEqual1 /////////////////////////////////////////////////

VarCode
ASTForm_NotEqual1::makeCode(SubstCode *subst)
{
  ASTTermCode *ct1 = t1->makeCode(subst);
  ASTTermCode *ct2 = t2->makeCode(subst);

  VarCode code = 
    andList(ct1->code,
	    codeTable.insert(new Code_Eq1(ct1->var, ct2->var, pos)),
	    ct2->code);
  
  VarCode res = 
    codeTable.insert(new Code_Negate(project(project(code, 
						     ct1, pos), 
					     ct2, pos),
				     pos));
  delete ct1;
  delete ct2;
  return res;
}

void 
ASTForm_NotEqual1::dump()
{
  cout << "NotEqual1("; t1->dump(); cout << ","; t2->dump(); cout << ")";
}
    
//////////  ASTForm_NotEqual2 /////////////////////////////////////////////////

VarCode
ASTForm_NotEqual2::makeCode(SubstCode *subst)
{
  ASTTermCode *ct1 = T1->makeCode(subst);
  ASTTermCode *ct2 = T2->makeCode(subst);

  VarCode code = 
    andList(ct1->code,
	    codeTable.insert(new Code_Eq2(ct1->var, ct2->var, pos)),
	    ct2->code);
  
  VarCode res = 
    codeTable.insert(new Code_Negate(project(project(code, 
						     ct1, pos), 
					     ct2, pos),
				     pos));
  delete ct1;
  delete ct2;
  return res;
}

void 
ASTForm_NotEqual2::dump()
{
  cout << "NotEqual2("; T1->dump(); cout << ","; T2->dump(); cout << ")";
}
    
//////////  ASTForm_Less //////////////////////////////////////////////////////

VarCode
ASTForm_Less::makeCode(SubstCode *subst)
{
  ASTTermCode *ct1 = t1->makeCode(subst);
  ASTTermCode *ct2 = t2->makeCode(subst);
  
  VarCode code = 
    andList(ct1->code,
	    codeTable.insert(new Code_Less1(ct1->var, ct2->var, pos)),
	    ct2->code);
  
  VarCode res = project(project(code, ct1, pos), ct2, pos);
  
  delete ct1;
  delete ct2;
  return res;
}

void 
ASTForm_Less::dump()
{
  cout << "Less("; t1->dump(); cout << ","; t2->dump(); cout << ")";
}
    
//////////  ASTForm_LessEq ////////////////////////////////////////////////////

VarCode
ASTForm_LessEq::makeCode(SubstCode *subst)
{
  ASTTermCode *ct1 = t1->makeCode(subst);
  ASTTermCode *ct2 = t2->makeCode(subst);
  
  VarCode code = 
    andList(ct1->code,
	    codeTable.insert(new Code_LessEq1(ct1->var, ct2->var, pos)),
	    ct2->code);
  
  VarCode res = project(project(code, ct1, pos), ct2, pos);

  delete ct1;
  delete ct2;
  return res;
}

void 
ASTForm_LessEq::dump()
{
  cout << "LessEq("; t1->dump(); cout << ","; t2->dump(); cout << ")";
}
    
//////////  ASTForm_Impl //////////////////////////////////////////////////////

VarCode
ASTForm_Impl::makeCode(SubstCode *subst)
{
  VarCode c1 = f1->makeCode(subst);
  VarCode c2 = f2->makeCode(subst);

  return codeTable.insert(new Code_Impl(c1, c2, pos));
}

void 
ASTForm_Impl::dump()
{
  cout << "Impl("; f1->dump(); cout << ","; f2->dump(); cout << ")";
}
    
//////////  ASTForm_Biimpl ////////////////////////////////////////////////////

VarCode
ASTForm_Biimpl::makeCode(SubstCode *subst)
{
  VarCode c1 = f1->makeCode(subst);
  VarCode c2 = f2->makeCode(subst);

  return codeTable.insert(new Code_Biimpl(c1, c2, pos));
}

void 
ASTForm_Biimpl::dump()
{
  cout << "Biimpl("; f1->dump(); cout << ","; f2->dump(); cout << ")";
}
    
//////////  ASTForm_And ///////////////////////////////////////////////////////

VarCode
ASTForm_And::makeCode(SubstCode *subst)
{
  VarCode c1 = f1->makeCode(subst);
  VarCode c2 = f2->makeCode(subst);

  return codeTable.insert(new Code_And(c1, c2, pos));
}

void 
ASTForm_And::dump()
{
  cout << "And("; f1->dump(); cout << ","; f2->dump(); cout << ")";
}
    
//////////  ASTForm_IdLeft ////////////////////////////////////////////////////

VarCode
ASTForm_IdLeft::makeCode(SubstCode *subst)
{
  VarCode c1 = f1->makeCode(subst);
  VarCode c2 = f2->makeCode(subst);

  return codeTable.insert(new Code_IdLeft(c1, c2, pos));
}

void 
ASTForm_IdLeft::dump()
{
  cout << "IdLeft("; f1->dump(); cout << ","; f2->dump(); cout << ")";
}
    
//////////  ASTForm_Or ////////////////////////////////////////////////////////

VarCode
ASTForm_Or::makeCode(SubstCode *subst)
{
  VarCode c1 = f1->makeCode(subst);
  VarCode c2 = f2->makeCode(subst);

  return codeTable.insert(new Code_Or(c1, c2, pos));
}

void 
ASTForm_Or::dump()
{
  cout << "Or("; f1->dump(); cout << ","; f2->dump(); cout << ")";
}
    
//////////  ASTForm_Not ///////////////////////////////////////////////////////

void 
ASTForm_Not::freeVars(IdentList *free, IdentList *bound)
{
  f->freeVars(free, bound);
}

VarCode
ASTForm_Not::makeCode(SubstCode *subst)
{
  VarCode c = f->makeCode(subst);

  return codeTable.insert(new Code_Negate(c, pos));
}

void 
ASTForm_Not::dump()
{
  cout << "Not("; f->dump(); cout << ")";
}
    
//////////  ASTForm_Ex0 ///////////////////////////////////////////////////////

VarCode
ASTForm_Ex0::makeCode(SubstCode *subst)
{
  return projectList(f->makeCode(subst), vl, pos);
}

void 
ASTForm_Ex0::dump()
{
  cout << "Ex0("; vl->dump(); cout << ","; f->dump(); cout << ")";
}
    
//////////  ASTForm_Ex1 ///////////////////////////////////////////////////////

VarCode
ASTForm_Ex1::makeCode(SubstCode *subst)
{
  IdentList *univs = substituteUnivs(subst, ul);

  IdentList::iterator i;
  for (i = vl->begin(); i != vl->end(); i++) 
    symbolTable.updateUnivs(*i, copy(univs));
  delete univs;

  return projectList(f->makeCode(subst), vl, pos);
}

void 
ASTForm_Ex1::dump()
{
  cout << "Ex1("; vl->dump(); cout << ","; f->dump(); cout << ")";
}
    
//////////  ASTForm_Ex2 ///////////////////////////////////////////////////////

VarCode
ASTForm_Ex2::makeCode(SubstCode *subst)
{
  IdentList *univs = substituteUnivs(subst, ul);

  IdentList::iterator i;
  for (i = vl->begin(); i != vl->end(); i++) 
    symbolTable.updateUnivs(*i, copy(univs));
  delete univs;

  return projectList(f->makeCode(subst), vl, pos);
}
  
void 
ASTForm_Ex2::dump()
{
  cout << "Ex2("; vl->dump(); cout << ","; f->dump(); cout << ")";
}
    
//////////  ASTForm_All0 //////////////////////////////////////////////////////

VarCode
ASTForm_All0::makeCode(SubstCode *subst)
{
  VarCode c = codeTable.insert(new Code_Negate(f->makeCode(subst), pos));

  return codeTable.insert(new Code_Negate(projectList(c, vl, pos), pos));
}

void 
ASTForm_All0::dump()
{
  cout << "All0("; vl->dump(); cout << ","; f->dump(); cout << ")";
}
    
//////////  ASTForm_All1 //////////////////////////////////////////////////////

VarCode
ASTForm_All1::makeCode(SubstCode *subst)
{
  IdentList *univs = substituteUnivs(subst, ul);

  IdentList::iterator i;
  for (i = vl->begin(); i != vl->end(); i++) 
    symbolTable.updateUnivs(*i, copy(univs));
  delete univs;

  VarCode c = codeTable.insert(new Code_Negate(f->makeCode(subst), pos));

  return codeTable.insert(new Code_Negate(projectList(c, vl, pos), pos));
}

void 
ASTForm_All1::dump()
{
  cout << "All1("; vl->dump(); cout << ","; f->dump(); cout << ")";
}
    
//////////  ASTForm_All2 //////////////////////////////////////////////////////

VarCode
ASTForm_All2::makeCode(SubstCode *subst)
{
  IdentList *univs = substituteUnivs(subst, ul);

  IdentList::iterator i;
  for (i = vl->begin(); i != vl->end(); i++) 
    symbolTable.updateUnivs(*i, copy(univs));
  delete univs;

  VarCode c = codeTable.insert(new Code_Negate(f->makeCode(subst), pos));

  return codeTable.insert
    (new Code_Negate(projectList(c, vl, pos), pos));
}

void 
ASTForm_All2::dump()
{
  cout << "All2("; vl->dump(); cout << ","; f->dump(); cout << ")";
}
    
//////////  ASTForm_Let0 //////////////////////////////////////////////////////

void 
ASTForm_Let0::freeVars(IdentList *free, IdentList *bound)
{
  IdentList::iterator ident;
  FormList::iterator form;
  
  for (ident = defIdents->begin(), form = defForms->begin() ; 
       ident != defIdents->end(); ident++, form++) {
    bound->insert(*ident);
    (*form)->freeVars(free, bound);
  }
  f->freeVars(free, bound);
}

VarCode
ASTForm_Let0::makeCode(SubstCode *subst)
{
  IdentList *idents = new IdentList;
  ASTForm *folded;

  folded = fold(idents);

  return projectList(folded->makeCode(subst), idents, pos); 
}

ASTForm*
ASTForm_Let0::fold(IdentList *idents)
{
  IdentList::iterator ident;
  FormList::iterator form;
  ASTForm *folded = f;

  if (defIdents->empty())
    return folded;

  ident = defIdents->end();
  ident--;
  form = defForms->end();
  form--;

  do {
    ASTForm *assign = new ASTForm_Biimpl(new ASTForm_Var0(*ident, pos), *form,
					 pos);
    folded = new ASTForm_And(assign, folded, pos);
    idents->push_front(*ident);
    form--;
  } while (ident-- != defIdents->begin());
  
  return folded;
}

void 
ASTForm_Let0::dump()
{
  IdentList::iterator ident;
  FormList::iterator form;

  cout << "Let0(";

  for (ident = defIdents->begin(), form = defForms->begin(); 
       ident != defIdents->end(); ident++, form++) {
    cout << "(Var0 " << symbolTable.lookupSymbol(*ident) 
	 << ","; (*form)->dump(); cout << "),";
  }

  f->dump(); cout << ")";
}
    
//////////  ASTForm_Let1 //////////////////////////////////////////////////////

void 
ASTForm_Let1::freeVars(IdentList *free, IdentList *bound)
{
  IdentList::iterator ident;
  Term1List::iterator term;
  
  for (ident = defIdents->begin(), term = defTerms->begin() ; 
       ident != defIdents->end(); ident++, term++) {
    bound->insert(*ident);
    (*term)->freeVars(free, bound);
  }
  f->freeVars(free, bound);
}

VarCode
ASTForm_Let1::makeCode(SubstCode *subst)
{
  IdentList *idents = new IdentList;
  ASTForm *folded;

  folded = fold(idents);

  return projectList(folded->makeCode(subst), idents, pos); 
}

ASTForm*
ASTForm_Let1::fold(IdentList *idents)
{
  IdentList::iterator ident;
  Term1List::iterator term;
  ASTForm *folded = f;

  if (defIdents->empty())
    return folded;

  ident = defIdents->end();
  ident--;
  term = defTerms->end();
  term--;

  do {
    folded = new ASTForm_And(new ASTForm_Bind1(*ident, *term, pos), 
			     folded, pos);
    idents->push_front(*ident);
    term--;
  } while (ident-- != defIdents->begin());
  
  return folded;
}

void 
ASTForm_Let1::dump()
{
  IdentList::iterator ident;
  Term1List::iterator term;

  cout << "Let1(";

  for (ident = defIdents->begin(), term = defTerms->begin(); 
       ident != defIdents->end(); ident++, term++) {
    cout << "(Var1 " << symbolTable.lookupSymbol(*ident) 
      << ","; (*term)->dump(); cout << "),";
  }

  f->dump(); cout << ")";
}
    
//////////  ASTForm_Let2 //////////////////////////////////////////////////////

void 
ASTForm_Let2::freeVars(IdentList *free, IdentList *bound)
{
  IdentList::iterator ident;
  Term2List::iterator term;
  
  for (ident = defIdents->begin(), term = defTerms->begin() ; 
       ident != defIdents->end(); ident++, term++) {
    bound->insert(*ident);
    (*term)->freeVars(free, bound);
  }
  f->freeVars(free, bound);
}

VarCode
ASTForm_Let2::makeCode(SubstCode *subst)
{
  IdentList *idents = new IdentList;
  ASTForm *folded;

  folded = fold(idents);

  return projectList(folded->makeCode(subst), idents, pos); 
}

ASTForm*
ASTForm_Let2::fold(IdentList *idents)
{
  IdentList::iterator ident;
  Term2List::iterator term;
  ASTForm *folded = f;

  if (defIdents->empty())
    return folded;

  ident = defIdents->end();
  ident--;
  term = defTerms->end();
  term--;

  do {
    folded = new ASTForm_And(new ASTForm_Bind2(*ident, *term, pos), 
			     folded, pos);
    idents->push_front(*ident);
    term--;
  } while (ident-- != defIdents->begin());
  
  return folded;
}

void 
ASTForm_Let2::dump()
{
  IdentList::iterator ident;
  Term2List::iterator term;

  cout << "Let2(";

  for (ident = defIdents->begin(), term = defTerms->begin(); 
       ident != defIdents->end(); ident++, term++) {
    cout << "(Var2 " << symbolTable.lookupSymbol(*ident) 
      << ","; (*term)->dump(); cout << "),";
  }

  f->dump(); cout << ")";
}
    
/////////  ASTForm_Bind1 //////////////////////////////////////////////////////

VarCode
ASTForm_Bind1::makeCode(SubstCode *subst)
{
  ASTTermCode *ct = t->makeCode(subst);

  // Ensure that n has same univList as t
  symbolTable.updateUnivs(n, copy(symbolTable.lookupUnivs(ct->var)));

  VarCode res = 
    project(andList(codeTable.insert(new Code_Eq1(n, ct->var, pos)),
		    ct->code),
	    ct, pos);

  delete ct;
  return res;
}

//////////  ASTForm_Bind2 /////////////////////////////////////////////////////

VarCode
ASTForm_Bind2::makeCode(SubstCode *subst)
{
  ASTTermCode *ct = T->makeCode(subst);

  // Ensure that n has same univList as T
  symbolTable.updateUnivs(n, copy(symbolTable.lookupUnivs(ct->var)));

  VarCode res =
    project(andList(codeTable.insert(new Code_Eq2(n, ct->var, pos)),
		    ct->code),
	    ct, pos);

  delete ct;
  return res;
}

//////////  ASTForm_Call //////////////////////////////////////////////////////

void 
ASTForm_Call::freeVars(IdentList *free, IdentList *bound)
{
  ASTList::iterator i;
  for (i = args->begin(); i != args->end(); i++)
    if ((*i)->order != oUniv)
      (*i)->freeVars(free, bound);

  PredLibEntry *p = predicateLib.lookup(n);
  bound->insert(p->bound);
  free->insert(p->frees);
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
	  symbolTable.updateRestriction(var, symbolTable.lookupRestriction(*formal));
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
	newsubst[k].termCode = substitute12UCode(subst, ((ASTUniv *) (*actual))->u);
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
	if (newsubst[k].termCode->code.vars !=
	    newsubst[k].termCode->code.code->vars)
	  delete newsubst[k].termCode->code.vars;
	newsubst[k].termCode->code.code->remove();
	delete newsubst[k].termCode;
	break;
      case sVarCode:
	if (newsubst[k].varCode->vars !=
	    newsubst[k].varCode->code->vars)
	  delete newsubst[k].varCode->vars;
	newsubst[k].varCode->code->remove();
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
      new Code_PredCall(n, dummyvc, actuals, p->frees, NULL, pos);

    if (codeTable.exists(*dummy)) 
      // equivalent node already in DAG
      res = codeTable.insert(dummy);

    else {
      char *filename = NULL;

      if (environment.separateCompilation) {
	// get separate compilation file name
	Deque<SSSet> *statespaces = new Deque<SSSet>;
	IdentList::iterator i;
	for (i = actuals.begin(); i != actuals.end(); i++) 
	  statespaces->push_back(stateSpaces(*i));
	for (i = p->frees->begin(); i != p->frees->end(); i++)
	  statespaces->push_back(stateSpaces(*i));

	filename = lib.getFileName(symbolTable.lookupSymbol(n),
				   p->source, dummy->sign, statespaces);
      }

      delete dummy->vars;
      delete dummy;

      if (environment.separateCompilation && lib.fileExists(filename))
	// found on disk
	res = codeTable.insert(new Code_External(filename, 
						 actuals, p->frees, pos));
      else { // need to make code

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
	  symbolTable.updateRestriction(*actual,symbolTable.lookupRestriction(*formal));
	}

	// make code for body
	VarCode vc = p->ast->makeCode(newsubst);
	delete[] newsubst;
	
	// insert code
	res = codeTable.insert(new Code_PredCall(n, vc, actuals, p->frees,
						 filename, pos));

	// restore the restrictions for the actuals
	Deque<ASTForm*>::iterator f;
	for (actual = actuals.begin(), f = oldRestrictions.begin();
	     actual != actuals.end(); actual++, f++)
	  symbolTable.updateRestriction(*actual, *f);
      }
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
	  
	  if (vc.vars != vc.code->vars)
	    delete vc.vars;
	  vc.code->remove();

	  res = fold(++iter, actuals, subst);
	}
	else {
	  Ident id = symbolTable.insertFresh(Varname0);
	  IdentList idl = IdentList(id);
	  actuals.push_back(id);
	  
	  res = projectList
	    (andList(fold(++iter, actuals, subst), 
		     codeTable.insert(new Code_Biimpl
				      (codeTable.insert(new Code_BoolVar
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

void 
ASTForm_Call::dump()
{
  cout << "Call(" << symbolTable.lookupSymbol(n);
  args->dump();
  cout << ")";
}

//////////  ASTForm_Import ///////////////////////////////////////////////////

void 
ASTForm_Import::freeVars(IdentList *free, IdentList *)
{
  free->insert(idents);
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

  return codeTable.insert(new Code_Import(file, fileVars, actuals, pos));
}

void 
ASTForm_Import::dump()
{
  cout << "Import(\"" << file << "\"";
  Deque<char *>::iterator i;
  IdentList::iterator j;
  for (i = fileVars->begin(), j = idents->begin(); 
       i != fileVars->end(); i++, j++)
    cout << ",(" << *i << "," << symbolTable.lookupSymbol(*j) << ")";
  cout << ")";
}

//////////  ASTForm_Export ///////////////////////////////////////////////////

void 
ASTForm_Export::freeVars(IdentList *free, IdentList *bound)
{
  f->freeVars(free, bound);
}

VarCode
ASTForm_Export::makeCode(SubstCode *subst)
{
  VarCode c = f->makeCode(subst);

  IdentList bound;
  IdentList *frees = new IdentList();
  f->freeVars(frees, &bound);
  if (lastPosVar != -1)
    frees->remove(lastPosVar);

  return codeTable.insert(new Code_Export(c, frees, file, pos));
}

void 
ASTForm_Export::dump()
{
  cout << "Export(\"" << file << "\","; f->dump(); cout << ")";
}

//////////  ASTForm_Prefix ///////////////////////////////////////////////////

void 
ASTForm_Prefix::freeVars(IdentList *free, IdentList *bound)
{
  f->freeVars(free, bound);
}

VarCode
ASTForm_Prefix::makeCode(SubstCode *subst)
{
  VarCode c = f->makeCode(subst);

  return codeTable.insert(new Code_Prefix(c, pos));
}

void 
ASTForm_Prefix::dump()
{
  cout << "Prefix("; f->dump(); cout << ")";
}

//////////  ASTForm_InStateSpace /////////////////////////////////////////////

void 
ASTForm_InStateSpace::freeVars(IdentList *free, IdentList *bound)
{
  t->freeVars(free, bound);
}

VarCode
ASTForm_InStateSpace::makeCode(SubstCode *subst)
{
  ASTTermCode *ct = t->makeCode(subst);

  VarCode res = 
    project(andList
	    (codeTable.insert(new Code_InStateSpace(ct->var, 
						    new IdentList(*ss), 
						    pos)),
	     ct->code),
	    ct, pos);

  delete ct;
  return res;
}

void 
ASTForm_InStateSpace::dump()
{
  cout << "InStateSpace("; t->dump(); 
  cout << ","; ss->dump(); cout << ")";
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
    vc = codeTable.insert(new Code_Project(*iter, vc, p));
  while (iter-- != projList->begin());

  return vc;
}

VarCode 
andList(VarCode vc1, VarCode vc2)
{
  if (vc1.code->kind == cTrue) {
    vc1.code->remove();
    return vc2;
  }
  else if (vc2.code->kind == cTrue) {
    vc2.code->remove();
    return vc1;
  }
  return codeTable.insert(new Code_And(vc1, vc2, vc1.code->pos));
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
    return codeTable.insert(new Code_Project(t->var, vc, p));
  else
    return vc;
}
