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

extern SymbolTable symbolTable;
extern PredicateLib predicateLib;

static IdentList restrVars2; // variables occuring in current restriction

void 
ASTTerm1_T::freeVars(IdentList *free, IdentList *bound)
{
  T->freeVars(free, bound);
}

void 
ASTTerm1_t::freeVars(IdentList *free, IdentList *bound)
{
  t->freeVars(free, bound);
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
ASTForm_f::freeVars(IdentList *free, IdentList *bound)
{
  f->freeVars(free, bound);
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

void 
ASTTerm1_Var1::freeVars(IdentList *free, IdentList *bound)
{
  if (!bound->exists(n))
    free->insert(n);
  
  if (!restrVars2.exists(n)) {
    restrVars2.insert(n);

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

    restrVars2.remove(n);
  }
}
  
void 
ASTTerm2_Var2::freeVars(IdentList *free, IdentList *bound)
{
  if (!bound->exists(n))
    free->insert(n);
  
  if (!restrVars2.exists(n)) {
    restrVars2.insert(n);

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
    
    restrVars2.remove(n);
  }
}

void 
ASTTerm2_VarTree::freeVars(IdentList *free, IdentList *bound)
{
  if (!bound->exists(n))
    free->insert(n);
  
  if (!restrVars2.exists(n)) {
    restrVars2.insert(n);

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

    restrVars2.remove(n);
  }
}

void 
ASTTerm2_Dot::freeVars(IdentList *free, IdentList *bound)
{
  T->freeVars(free, bound);
}

void 
ASTTerm2_Up::freeVars(IdentList *free, IdentList *bound)
{
  T->freeVars(free, bound);
}

void 
ASTTerm2_Set::freeVars(IdentList *free, IdentList *bound)
{
  ASTList::iterator i;
  for (i = elements->begin(); i != elements->end(); i++)
    (*i)->freeVars(free, bound);
}
  
void 
ASTTerm2_Interval::freeVars(IdentList *free, IdentList *bound)
{
  t1->freeVars(free, bound);
  t2->freeVars(free, bound);
}
  
void 
ASTTerm2_Formula::freeVars(IdentList *free, IdentList *bound)
{
  bound->insert(fresh);
  f->freeVars(free, bound);
}
  
void 
ASTForm_Var0::freeVars(IdentList *free, IdentList *bound)
{
  if (!bound->exists(n))
    free->insert(n);
}

void 
ASTForm_RootPred::freeVars(IdentList *free, IdentList *bound)
{
  t->freeVars(free, bound);
}

void 
ASTForm_FirstOrder::freeVars(IdentList *free, IdentList *bound)
{
  t->freeVars(free, bound);
}

void 
ASTForm_Not::freeVars(IdentList *free, IdentList *bound)
{
  f->freeVars(free, bound);
}

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

void 
ASTForm_Import::freeVars(IdentList *free, IdentList *)
{
  free->insert(idents);
}

void 
ASTForm_InStateSpace1::freeVars(IdentList *free, IdentList *bound)
{
  t->freeVars(free, bound);
}

void 
ASTForm_InStateSpace2::freeVars(IdentList *free, IdentList *bound)
{
  T->freeVars(free, bound);
}

void 
ASTForm_SomeType::freeVars(IdentList *free, IdentList *bound)
{
  t->freeVars(free, bound);
}
