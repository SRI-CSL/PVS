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

#include "codetable.h"
#include "env.h"

#define ENABLE_PROJECT
#define ENABLE_LOCAL
#define ENABLE_AND

extern Options options;
extern CodeTable *codeTable;

// DAG TRAVERSAL

static int phase = 0;

void 
VarCode::reduceAll(Deque<VarCode> *vcl) 
{
 unmark();

 phase = 1; 
 reduce();
 for (Deque<VarCode>::iterator i = vcl->begin(); i != vcl->end(); i++)
   (*i).reduce();
 unmark();
 for (Deque<VarCode>::iterator i = vcl->begin(); i != vcl->end(); i++)
   (*i).unmark();
 codeTable->clearSCTable();

#ifdef ENABLE_AND
 phase = 2; 
 reduce();
 for (Deque<VarCode>::iterator i = vcl->begin(); i != vcl->end(); i++)
   (*i).reduce();
 unmark();
 for (Deque<VarCode>::iterator i = vcl->begin(); i != vcl->end(); i++)
   (*i).unmark();
#endif
}

void 
VarCode::reduce() 
{
  if (!code->mark) { // first time at this node?
    code->mark = true;
    switch (phase) { // reduce node 
    case 1:
      code->reduce1(); // local reductions + projection reduction
      break;
    case 2:
      code->reduce2(); // and reduction
      break;
    }
    if (!code->forwarded.code) { // if not already forwarded...
      Code *c = codeTable->findEquiv(code);
      if (c) { // ...but isomorphic to some other node
	c->refs++; // (because sub-nodes have changed)
	code->forwarded = VarCode(code->vars.copy(), c); // forward to other
      }
    }
  }
  if (code->forwarded.code) { // redirect if forwarded
    VarCode old = *this;
    vars = subst(code->forwarded.vars, &code->vars, vars); // update vars
    vars->compress();
    code = code->forwarded.code;
    code->refs++;
    old.remove(); // remove reference to old node
    reduce(); // again (so that sub-nodes always have been processed)
  }
  invariant(vars->size() == code->vars.size());
  invariant(!code->forwarded.code); // no successive forwarding
}

// GENERIC REDUCTION DUMMIES

void
Code_c::reduce1() 
{
  vc.reduce();
}

void
Code_cc::reduce1() 
{
  vc1.reduce();
  vc2.reduce();
}

// LEAF REDUCTIONS

void 
Code_Eq1::reduce1() 
{
#ifdef ENABLE_LOCAL
  // p = p --> true
  if (id1 == id2) {
    forwarded = codeTable->insert(new Code_True(pos));
    codeTable->red_other++;
  }
#endif
}

void 
Code_Eq2::reduce1() 
{
#ifdef ENABLE_LOCAL
  // P = P --> true
  if (id1 == id2) {
    forwarded = codeTable->insert(new Code_True(pos));
    codeTable->red_other++;
  }
#endif
}

// PROJECTION REDUCTIONS

void 
Code_Project::reduce1() 
{
  Code_c::reduce1();

#ifdef ENABLE_PROJECT
  // ex x: phi --> phi[y/x] if phi = .. & x=y & .. where x!=y
  //                           and y already in all affected export-vars
  Ident y = findEquality(var, vc);
  vc.code->clearEqlist();
  if (y != -1 && !checkExport(var))
    y = -1; /* cancel */
  clearEqlist();
  if (y != -1) {
    invariant(y != var);
    IdentList formals(var), actuals(y);
    forwarded = vc.substCopy(&formals, &actuals);
    forwarded.reduce();
  }
  // ex x: phi --> phi if x notin FV(phi)
  else if (!vc.vars->exists(var)) {
    vc.code->refs++;
    forwarded = VarCode(vc.vars->copy(), vc.code);
  }
  if (forwarded.code)
    codeTable->red_proj++;
#endif
}

Ident
Code_Project::findEquality(Ident x, VarCode vc)
{
  Ident y = -1;
  if (!vc.vars->exists(x))
    return -1;
  x = subst(x, vc.vars, &vc.code->vars);
  if (vc.code->eqlist && vc.code->eqlist->exists(x))
    return -1;
  switch (vc.code->kind) {
  case cAnd: {
    Code_And *c = (Code_And *) vc.code;
    y = findEquality(x, c->vc1);
    if (y == -1) 
      y = findEquality(x, c->vc2);
    break; 
  }
  case cProject:
  case cRestrict:
  case cPredCall: {
    Code_c *c = (Code_c *) vc.code;
    y = findEquality(x, c->vc);
    break; 
  }
  case cEq1:
  case cEq2: {
    Code_nn *c = (Code_nn *) vc.code;
    if (c->id1 == x && c->id2 != x && sameUnivs(c->id1, c->id2))
      y = c->id2;
    else if (c->id2 == x && c->id1 != x && sameUnivs(c->id1, c->id2))
      y = c->id1;
    break;
  }
  case cBiimpl: {
    Code_Biimpl *c = (Code_Biimpl *) vc.code;
    if (c->vc1.code->kind == cBoolVar &&
	c->vc2.code->kind == cBoolVar) {
      Code_BoolVar *c1 = (Code_BoolVar *) c->vc1.code;
      Code_BoolVar *c2 = (Code_BoolVar *) c->vc2.code;
      if (c1->id == x && c2->id != x)
	y = c2->id;
      else if (c2->id == x && c1->id != x)
	y = c1->id;
    }
    break;
  }
  default: ; // no equality found
  }
  if (y != -1)
    y = subst(y, &vc.code->vars, vc.vars);
  if (!vc.code->eqlist)
    vc.code->eqlist = new IdentList;
  vc.code->eqlist->insert(x);
  return y;
}

void
Code::clearEqlist()
{
  if (eqlist) {
    delete eqlist;
    eqlist = NULL;
  }
}
void
Code_c::clearEqlist()
{
  if (eqlist) {
    delete eqlist;
    eqlist = NULL;
    vc.code->clearEqlist();
  }
}

void
Code_cc::clearEqlist()
{
  if (eqlist) {
    delete eqlist;
    eqlist = NULL;
    vc1.code->clearEqlist();
    vc2.code->clearEqlist();
  }
}

bool
Code::checkExport(Ident)
{
  return true;
}

bool
Code_c::checkExport(Ident x)
{
  if (eqlist && eqlist->exists(x))
    return true;
  if (!eqlist)
    eqlist = new IdentList;
  eqlist->insert(x);
  return vc.code->checkExport(subst(x, vc.vars, &vc.code->vars));
}

bool
Code_cc::checkExport(Ident x)
{
  if (eqlist && eqlist->exists(x))
    return true;
  if (!eqlist)
    eqlist = new IdentList;
  eqlist->insert(x);
  return vc1.code->checkExport(subst(x, vc1.vars, &vc1.code->vars)) && 
    vc2.code->checkExport(subst(x, vc2.vars, &vc2.code->vars));
}

bool
Code_Export::checkExport(Ident x)
{
  if (eqlist && eqlist->exists(x))
    return true;
  if (!eqlist)
    eqlist = new IdentList;
  eqlist->insert(x);
  if (!vc.code->checkExport(subst(x, vc.vars, &vc.code->vars)))
    return false;
  IdentList::iterator i;
  for (i = freevars.begin(); i != freevars.end(); i++)
    if (*i == x)
      return false; // reduction would cause export free vars to change
  return true;
}

// LOCAL NEGATION REDUCTIONS

void 
Code_Negate::reduce1() 
{
  Code_c::reduce1();
  
#ifdef ENABLE_LOCAL
  // ~~X --> X
  if (vc.code->kind == cNegate) {
    Code_Negate *c = (Code_Negate *) vc.code;
    c->vc.code->refs++;
    forwarded = VarCode(vc.vars->copy(), c->vc.code);
  }
  // ~true --> false
  else if (vc.code->kind == cTrue)
    forwarded = codeTable->insert(new Code_False(pos));
  // ~false --> true
  else if (vc.code->kind == cFalse)
    forwarded = codeTable->insert(new Code_True(pos));
  if (forwarded.code)
    codeTable->red_other++;
#endif
}

// PREDCALL REDUCTION

void 
Code_PredCall::reduce1() 
{
  Code_c::reduce1();

#ifdef FOOBAR  
#ifdef ENABLE_LOCAL
  if (!options.separateCompilation) {
    vc.code->refs++;
    forwarded = VarCode(vc.vars->copy(), vc.code);
    codeTable->red_other++;
  }
#endif
#endif
}

// LOCAL PRODUCT REDUCTIONS

void 
Code_And::reduce1() 
{
  Code_cc::reduce1();

#ifdef ENABLE_LOCAL
  // true & X --> X
  if (vc1.code->kind == cTrue) {
    vc2.code->refs++;
    forwarded = VarCode(vc2.vars->copy(), vc2.code);
  }
  // X & true --> X
  else if (vc2.code->kind == cTrue) {
    vc1.code->refs++;
    forwarded = VarCode(vc1.vars->copy(), vc1.code);
  }
  // X & false --> false
  else if (vc2.code->kind == cFalse) {
    vc2.code->refs++;
    forwarded = VarCode(vc2.vars->copy(), vc2.code);
  }
  // false & X --> false
  else if (vc1.code->kind == cFalse) {
    vc1.code->refs++;
    forwarded = VarCode(vc1.vars->copy(), vc1.code);
  }
  // X & X --> X
  else if (vc1.code == vc2.code && 
	   equal(vc1.vars, vc2.vars)) {
    vc1.code->refs++;
    forwarded = VarCode(vc1.vars->copy(), vc1.code);
  }
  if (forwarded.code)
    codeTable->red_prod++;
#endif
}

void 
Code_Or::reduce1() 
{
  Code_cc::reduce1();

#ifdef ENABLE_LOCAL
  // false | X --> X
  if (vc1.code->kind == cFalse) {
    vc2.code->refs++;
    forwarded = VarCode(vc2.vars->copy(), vc2.code);
  }
  // X | false --> X
  else if (vc2.code->kind == cFalse) {
    vc1.code->refs++;
    forwarded = VarCode(vc1.vars->copy(), vc1.code);
  }
  // X | true --> true
  else if (vc2.code->kind == cTrue) {
    vc2.code->refs++;
    forwarded = VarCode(vc2.vars->copy(), vc2.code);
  }
  // true | X --> true
  else if (vc1.code->kind == cTrue) {
    vc1.code->refs++;
    forwarded = VarCode(vc1.vars->copy(), vc1.code);
  }
  // X | X --> X
  else if (vc1.code == vc2.code && 
	   equal(vc1.vars, vc2.vars)) {
    vc1.code->refs++;
    forwarded = VarCode(vc1.vars->copy(), vc1.code);
  }
  if (forwarded.code)
    codeTable->red_prod++;
#endif
}

void 
Code_Impl::reduce1() 
{
  Code_cc::reduce1();

#ifdef ENABLE_LOCAL
  // true => X --> X
  if (vc1.code->kind == cTrue) {
    vc2.code->refs++;
    forwarded = VarCode(vc2.vars->copy(), vc2.code);
  }
  // X => true --> true
  else if (vc2.code->kind == cTrue)
    forwarded = codeTable->insert(new Code_True(pos));
  // X => false --> ~X
  else if (vc2.code->kind == cFalse) {
    vc1.code->refs++;
    forwarded = codeTable->insert(new Code_Negate
				 (VarCode(vc1.vars->copy(), vc1.code), pos));
  }
  // false => X --> true
  else if (vc1.code->kind == cFalse)
    forwarded = codeTable->insert(new Code_True(pos));
  // X => X --> true
  else if (vc1.code == vc2.code && 
	   equal(vc1.vars, vc2.vars))
    forwarded = codeTable->insert(new Code_True(pos));
  if (forwarded.code)
    codeTable->red_prod++;
#endif
}

void 
Code_Biimpl::reduce1() 
{
  Code_cc::reduce1();

#ifdef ENABLE_LOCAL
  // true <=> X --> X
  if (vc1.code->kind == cTrue) {
    vc2.code->refs++;
    forwarded = VarCode(vc2.vars->copy(), vc2.code);
  }
  // X <=> true --> X
  else if (vc2.code->kind == cTrue) {
    vc1.code->refs++;
    forwarded = VarCode(vc1.vars->copy(), vc1.code);
  }
  // X <=> false --> ~X
  else if (vc2.code->kind == cFalse) {
    vc1.code->refs++;
    forwarded = codeTable->insert(new Code_Negate
				 (VarCode(vc1.vars->copy(), vc1.code), pos));
  }
  // false <=> X --> ~X
  else if (vc1.code->kind == cFalse) {
    vc2.code->refs++;
    forwarded = codeTable->insert(new Code_Negate
				 (VarCode(vc2.vars->copy(), vc2.code), pos));
  }
  // X <=> X --> true
  else if (vc1.code == vc2.code && 
	   equal(vc1.vars, vc2.vars))
    forwarded = codeTable->insert(new Code_True(pos));
  if (forwarded.code)
    codeTable->red_prod++;
#endif
}

// DAG (UN)MARKING

void
VarCode::unmark()
{
  setmark(2);
  setmark(0);
}

void
VarCode::setmark(int val)
{
  code->setmark(val);
  code->sclist.reset(); // reset sclists
}

void
Code::setmark(int val)
{
  mark = val;
}

void
Code_c::setmark(int val)
{
  if (mark != val) {
    mark = val;
    vc.setmark(val);
  }
}

void
Code_cc::setmark(int val)
{
  if (mark != val) {
    mark = val;
    vc1.setmark(val);
    vc2.setmark(val);
  }
}

// GLOBAL PRODUCT REDUCTIONS

void
Code::reduce2()
{
  conj = new VarCodeList;
  conj->insert(vars.copy(), this);
}

void
Code_c::reduce2()
{
  Code::reduce2();
  vc.reduce();
}

void
Code_cc::reduce2()
{
  Code::reduce2();
  vc1.reduce();
  vc2.reduce();
}

void
Code_Restrict::reduce2()
{
  vc.reduce();

  VarCodeList::iterator i;
  restrconj = new VarCodeList;
  if (vc.code->conj)
    for (i = vc.code->conj->begin(); i != vc.code->conj->end(); i++)
      restrconj->insert(subst((*i).vars, &vc.code->vars, vc.vars), (*i).code);
  if (vc.code->restrconj)
    for (i = vc.code->restrconj->begin(); i != vc.code->restrconj->end(); i++)
      restrconj->insert(subst((*i).vars, &vc.code->vars, vc.vars), (*i).code);
}

void
Code_PredCall::reduce2()
{
  vc.reduce();

  VarCodeList::iterator i;
  conj = new VarCodeList;
  restrconj = new VarCodeList;
  if (vc.code->conj)
    for (i = vc.code->conj->begin(); i != vc.code->conj->end(); i++)
      conj->insert(subst((*i).vars, &vc.code->vars, vc.vars), (*i).code);
  if (vc.code->restrconj)
    for (i = vc.code->restrconj->begin(); i != vc.code->restrconj->end(); i++)
      restrconj->insert(subst((*i).vars, &vc.code->vars, vc.vars), (*i).code);
}

void
Code_Project::reduce2()
{
  Code_c::reduce2();

  // for obscure reasons it is safe to add sublists
  VarCodeList::iterator i;
  restrconj = new VarCodeList;
  if (vc.code->conj)
    for (i = vc.code->conj->begin(); i != vc.code->conj->end(); i++)
      conj->insert(subst((*i).vars, &vc.code->vars, vc.vars), (*i).code);
  if (vc.code->restrconj)
    for (i = vc.code->restrconj->begin(); i != vc.code->restrconj->end(); i++)
      restrconj->insert(subst((*i).vars, &vc.code->vars, vc.vars), (*i).code);
}

void
Code_Negate::reduce2()
{
  Code_c::reduce2();

  // for obscure reasons it is safe to add sublists for negate-project-negate
  if (vc.code->kind==cProject &&
      ((Code_Project *) vc.code)->vc.code->kind==cNegate) {
    Code_Project *proj = (Code_Project *) vc.code;
    Code_Negate *neg = (Code_Negate *) proj->vc.code;
    VarCodeList::iterator i;
    restrconj = new VarCodeList;
    if (neg->vc.code->conj)
      for (i = neg->vc.code->conj->begin(); 
	   i != neg->vc.code->conj->end(); i++) {
	IdentList *t1 = subst((*i).vars, &neg->vc.code->vars, neg->vc.vars);
	IdentList *t2 = subst(t1, &proj->vc.code->vars, proj->vc.vars);
	conj->insert(subst(t2, &vc.code->vars, vc.vars), (*i).code);
	delete t1;
	delete t2;
      }
    if (neg->vc.code->restrconj)
      for (i = neg->vc.code->restrconj->begin(); 
	   i != neg->vc.code->restrconj->end(); i++) {
	IdentList *t1 = subst((*i).vars, &neg->vc.code->vars, neg->vc.vars);
	IdentList *t2 = subst(t1, &proj->vc.code->vars, proj->vc.vars);
	restrconj->insert(subst(t2, &vc.code->vars, vc.vars), (*i).code);
	delete t1;
	delete t2;
      }
  }
}

void
Code_And::reduce2()
{
  vc1.reduce();
  vc2.reduce();

  VarCodeList::iterator i;
  conj = new VarCodeList;
  restrconj = new VarCodeList;
  VarCodeList t1, t2;
  if (vc1.code->conj)
    for (i = vc1.code->conj->begin(); i != vc1.code->conj->end(); i++)
      conj->insert(subst((*i).vars, &vc1.code->vars, vc1.vars), (*i).code);
  if (vc1.code->restrconj)
    for (i = vc1.code->restrconj->begin(); i != vc1.code->restrconj->end(); i++)
      restrconj->insert(subst((*i).vars, &vc1.code->vars, vc1.vars), (*i).code);
  if (vc2.code->conj)
    for (i = vc2.code->conj->begin(); i != vc2.code->conj->end(); i++)
      t1.insert(subst((*i).vars, &vc2.code->vars, vc2.vars), (*i).code);
  if (vc2.code->restrconj)
    for (i = vc2.code->restrconj->begin(); i != vc2.code->restrconj->end(); i++)
      t2.insert(subst((*i).vars, &vc2.code->vars, vc2.vars), (*i).code);

  // X & Y --> X  iff  Y_conj \sub (X_conj \cup X_restrconj) 
  //                   and Y_restrconj \sub X_restrconj
  if (t1.sub(conj, restrconj) && t2.sub(restrconj)) {
    vc1.code->refs++;
    forwarded = VarCode(vc1.vars->copy(), vc1.code);
  }
  // X & Y --> Y  iff  X_conj \sub (Y_conj \cup Y_restrconj) 
  //                   and X_restrconj \sub Y_restrconj
  else if (conj->sub(&t1, &t2) && restrconj->sub(&t2)) {
    vc2.code->refs++;
    forwarded = VarCode(vc2.vars->copy(), vc2.code);
  }

  conj->insert(&t1);
  restrconj->insert(&t2);
  if (forwarded.code)
    codeTable->red_prod++;
}
