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

#include <iostream.h>
#include <string.h>
#include "codetable.h"
#include "offsets.h"
#include "predlib.h"
#include "symboltable.h"
#include "env.h"
#include "lib.h"

extern "C" {
#include "../Mem/mem.h"
}

extern Options options;
extern Offsets offsets;
extern PredicateLib predicateLib;
extern SymbolTable symbolTable;
extern CodeTable *codeTable;
extern AutLib lib;

Ident lastPosVar, allPosVar;

/////////// stateSpaces ///////////////////////////////////////////////////////

SSSet
stateSpaces(IdentList *univs) // find state spaces for univs
{
  SSSet set = (SSSet) mem_alloc(guide.numSs);

  for (unsigned i = 0; i < guide.numSs; i++)
    set[i] = 0; // clear all

  if (univs)
    for (Ident *id = univs->begin(); id != univs->end(); id++) {
      switch (symbolTable.lookupType(*id)) {
      case Univname: {
	int u = symbolTable.lookupUnivNumber(*id);
	for (unsigned i = 0; i < guide.numUnivSS[u]; i++)
	  set[guide.univSS[u][i]] = 1;
	break;
      }
      case Statespacename:
	set[symbolTable.lookupNumber(*id)] = 1;
	break;
      default: 
	invariant(0);
      }
    }
 
  return set;
}

SSSet
stateSpaces(Ident id) // find state spaces for id
{
  return stateSpaces(symbolTable.lookupUnivs(id));
}

/////////// VarCode ///////////////////////////////////////////////////////////

DFA* 
VarCode::DFATranslate()
{
  if (!code->dfa) {
    invariant(!code->mark);
    if (options.intermediate)
      code->show();
    code->makeDFA();
    code->mark = true;
    invariant(code->dfa);
    codeTable->print_progress();
  }

  DFA *a = code->dfa;
  if (code->refs > 1)
    a = st_dfa_copy(a);
  else
    code->dfa = NULL;
  st_dfa_replace_indices(a, vars, &code->vars); 
  return a;
}

GTA* 
VarCode::GTATranslate()
{
  if (!code->gta) {
    invariant(!code->mark);
    if (options.intermediate)
      code->show();
    code->makeGTA();
    code->mark = true;
    invariant(code->gta);
    codeTable->print_progress();
  }

  GTA *g = code->gta;
  if (code->refs > 1)
    g = st_gta_copy(g);
  else
    code->gta = NULL;
  st_gta_replace_indices(g, vars, &code->vars); 
  return g;
}

void
VarCode::remove()
{
  if (code) {
    if (vars != &code->vars)
      delete vars;
    code->remove();
    code = NULL;
  }
}

VarCodeList::~VarCodeList()
{
  iterator i;
  for (i = begin(); i != end(); i++)
    delete (*i).vars;
}

void
VarCodeList::insert(IdentList *v, Code *c)
{
  VarCodeList::iterator i;
  for (i = begin(); i != end(); i++)
    if ((*i).code == c && equal((*i).vars, v)) {
      delete v;
      return;
    }
  push_back(VarCode(v, c));
}

void
VarCodeList::insert(VarCodeList *l)
{ // move all elements from l to this
  VarCodeList::iterator i;
  for (i = l->begin(); i != l->end(); i++)
    insert((*i).vars, (*i).code);
  l->reset();
}

bool 
VarCodeList::sub(VarCodeList *l1, VarCodeList *l2)
{ // check this subset of l1 union l2
  VarCodeList::iterator i, j = 0;
  for (i = begin(); i != end(); i++) {
    if (l1)
      for (j = l1->begin(); j != l1->end(); j++)
	if ((*i).code == (*j).code && equal((*i).vars, (*j).vars))
	  break;
    if (!l1 || j == l1->end()) {
      if (l2)
	for (j = l2->begin(); j != l2->end(); j++)
	  if ((*i).code == (*j).code && equal((*i).vars, (*j).vars))
	    break;
      if (!l2 || j == l2->end())
	return false;
    }
  }
  return true;
}

/////////// Code //////////////////////////////////////////////////////////////

bool
Code::equiv(Code &c)
{
  return kind == c.kind;
}

unsigned
Code::hash()
{
  return kind;
}

void
Code::remove() // remove one reference to this node
{
  invariant(refs > 0);
  if (--refs == 0) {
    if (!mark) // node skipped due to sep. comp.
      codeTable->makes++;
    if (dfa)
      dfaFree(dfa);
    if (gta)
      gtaFree(gta);
    if (conj)
      delete conj;
    if (restrconj)
      delete restrconj;
    codeTable->remove(this);
    forwarded.remove();
    delete this;
  }
}

void
Code::error(String str)
{
  cout << "\nError:";
  pos.printsource();
  cout << "\n" << str << "\n" << "Execution aborted\n";  
  exit(-1);
}

////////// Code_n /////////////////////////////////////////////////////////////

Code_n::Code_n(CodeKind knd, Ident i, Pos p) :
  Code(knd, p), id(i)
{
  vars.push_back(id);
}

bool
Code_n::equiv(Code &c)
{
  return kind == c.kind &&
    sameUnivs(id, ((Code_n&) c).id);
}

unsigned
Code_n::hash() 
{
  return kind;
}

////////// Code_ni ////////////////////////////////////////////////////////////

Code_ni::Code_ni(CodeKind knd, Ident i, int n, Pos p) :
  Code_n(knd, i, p)
{
  val = n;
}

bool
Code_ni::equiv(Code &c)
{
  return kind == c.kind && 
    val == ((Code_ni&) c).val &&
    sameUnivs(id, ((Code_ni&) c).id); // signatures trivially identical
}

unsigned
Code_ni::hash()
{
  return kind + val*128;
}

////////// Code_nn ////////////////////////////////////////////////////////////

Code_nn::Code_nn(CodeKind knd, Ident i, Ident j, Pos p) :
  Code(knd, p), id1(i), id2(j)
{
  if ((kind==cEq1 || kind==cEq2) & id1 > id2) { // exploit relation symmetry
    Ident t = id1;
    id1 = id2;
    id2 = t;
  }

  vars.push_back(id1);
  vars.push_back(id2);
  sign.make(vars);
  vars.compress();
}

bool
Code_nn::equiv(Code &c)
{
  return kind == c.kind && 
    sign == ((Code_nn&) c).sign &&
    sameUnivs(id1, ((Code_nn&) c).id1) &&
    sameUnivs(id2, ((Code_nn&) c).id2); 
}

unsigned
Code_nn::hash() 
{
  return kind + sign.hashvalue*3;
}

////////// Code_nni ///////////////////////////////////////////////////////////

Code_nni::Code_nni(CodeKind knd, Ident i, Ident j, int n, Pos p) :
  Code_nn(knd, i, j, p)
{
  val = n;
}

bool
Code_nni::equiv(Code &c)
{
  return kind == c.kind && 
    sign == ((Code_nni&) c).sign &&
    val == ((Code_nni&) c).val &&
    sameUnivs(id1, ((Code_nni&) c).id1) &&
    sameUnivs(id2, ((Code_nni&) c).id2); 
}

unsigned
Code_nni::hash() 
{
  return kind + sign.hashvalue*3 + val*128;
}

////////// Code_nnn ///////////////////////////////////////////////////////////

Code_nnn::Code_nnn(CodeKind knd, Ident i, Ident j, Ident k, Pos p) :
  Code(knd, p), id1(i), id2(j), id3(k)
{
  if ((kind==cEqUnion || kind==cEqInter) & id2 > id3) { // exploit symmetry
    Ident t = id2;
    id2 = id3;
    id3 = t;
  }
  
  vars.push_back(id1);
  vars.push_back(id2);
  vars.push_back(id3);
  sign.make(vars);
  vars.compress();
}

bool
Code_nnn::equiv(Code &c)
{
  return kind == c.kind &&
    sign == ((Code_nnn&) c).sign &&
    sameUnivs(id1, ((Code_nnn&) c).id1) &&
    sameUnivs(id2, ((Code_nnn&) c).id2) &&
    sameUnivs(id3, ((Code_nnn&) c).id3); 
}

unsigned 
Code_nnn::hash() 
{
  return kind + sign.hashvalue*3;
}

////////// Code_c /////////////////////////////////////////////////////////////

Code_c::Code_c(CodeKind knd, VarCode c, Pos p, bool skipinit) :
  Code(knd, p), vc(c)
{
  if (!skipinit) {
    vars.append(vc.vars);
    sign.make(vars);
    vars.compress();
  }
}

bool
Code_c::equiv(Code &c)
{
  return kind == c.kind && 
    vc.code == ((Code_c&) c).vc.code &&
    sign == ((Code_c&) c).sign;
}

unsigned 
Code_c::hash() 
{
  return kind + ((int) vc.code)*128;
}

////////// Code_cc ////////////////////////////////////////////////////////////

Code_cc::Code_cc(CodeKind knd, VarCode c1, VarCode c2, Pos p):
  Code(knd, p), vc1(c1), vc2(c2) 
{
  if (kind==cAnd || kind==cOr || kind==cBiimpl) // exploit symmetry
    if (vc1.code < vc2.code) {
      VarCode t = vc1;
      vc1 = vc2;
      vc2 = t;
    }
  
  vars.append(vc1.vars);
  vars.append(vc2.vars);
  sign.make(vars);
  vars.compress();
}

bool
Code_cc::equiv(Code &c)
{
  return kind == c.kind && 
    vc1.code == ((Code_cc&) c).vc1.code && 
    vc2.code == ((Code_cc&) c).vc2.code &&
    sign == ((Code_cc&) c).sign;
}

unsigned 
Code_cc::hash() 
{
  return kind*2 + ((int) vc1.code)*5 + ((int) vc2.code)*7;
}

////////// Atomic formulas ////////////////////////////////////////////////////

void
Code_True::makeDFA()
{
  dfa = dfaTrue();
}

void
Code_True::makeGTA()
{
  gta = gtaTrue();
}

void 
Code_False::makeDFA()
{
  dfa = dfaFalse();
}

void 
Code_False::makeGTA()
{
  gta = gtaFalse();
}

void 
Code_EqEmpty::makeDFA()
{
  dfa = dfaEmpty(offsets.off(id));
}

void 
Code_EqEmpty::makeGTA()
{
  gta = gtaEmpty(offsets.off(id), stateSpaces(id));
}

bool
Code_EqRoot::equiv(Code &c)
{
  return kind == c.kind &&
    sameUnivs(id, ((Code_n&) c).id)&&
    equal(universes, ((Code_EqRoot&) c).universes);
}

void 
Code_EqRoot::makeGTA()
{
  SSSet u = stateSpaces(universes);
  for (unsigned i = 0; i < guide.numSs; i++)
    if (!guide.ssUnivRoot[i])
	u[i] = 0; // remove non-roots
  gta = gtaRoot(offsets.off(id), stateSpaces(id), u);
}

void 
Code_FirstOrder::makeDFA()
{
  dfa = dfaFirstOrder(offsets.off(id));
}

void 
Code_FirstOrder::makeGTA()
{
  gta = gtaFirstOrder(offsets.off(id), stateSpaces(id));
}

void 
Code_EqConst::makeDFA()
{
  dfa = dfaConst(val, offsets.off(id));
}

void 
Code_Singleton::makeDFA()
{
  dfa = dfaSingleton(offsets.off(id));
}

void 
Code_Singleton::makeGTA()
{
  gta = gtaSingleton(offsets.off(id), stateSpaces(id));
}

void 
Code_BoolVar::makeDFA()
{
  dfa = dfaBoolvar(offsets.off(id));
}

void 
Code_BoolVar::makeGTA()
{
  gta = gtaBoolvar(offsets.off(id));
}

bool 
Code_InStateSpace::equiv(Code &c)
{
  return kind == c.kind && 
    equal(ss, ((Code_InStateSpace&) c).ss) &&
    sameUnivs(id, ((Code_InStateSpace&) c).id); // sign. trivially identical
}

unsigned 
Code_InStateSpace::hash()
{
  return kind + ss->hash();
}

void 
Code_InStateSpace::makeGTA()
{
  SSSet set = (SSSet) mem_alloc(guide.numSs);
  for (unsigned i = 0; i < guide.numSs; i++)
    set[i] = 0;
  for (Ident *s = ss->begin(); s != ss->end(); s++)
    set[symbolTable.lookupNumber(*s)] = 1;

  gta = gtaInStateSpace(offsets.off(id), set, stateSpaces(id));
}

void 
Code_SomeType::makeGTA()
{
  gta = gtaSomeType(offsets.off(id), stateSpaces(id));
}

void 
Code_In::makeDFA()
{
  dfa = dfaIn(offsets.off(id1), offsets.off(id2));
}

void 
Code_In::makeGTA()
{
  gta = gtaIn(offsets.off(id1), offsets.off(id2), 
	      stateSpaces(id1), stateSpaces(id2));
}

void 
Code_Eq1::makeDFA()
{
  dfa = dfaEq1(offsets.off(id1), offsets.off(id2));
}

void 
Code_Eq1::makeGTA()
{
  gta = gtaEq1(offsets.off(id1), offsets.off(id2), 
	       stateSpaces(id1), stateSpaces(id2));
}

void 
Code_Eq2::makeDFA()
{
  dfa = dfaEq2(offsets.off(id1), offsets.off(id2));
}

void 
Code_Eq2::makeGTA()
{
  gta = gtaEq2(offsets.off(id1), offsets.off(id2), 
	       stateSpaces(id1), stateSpaces(id2));
}

void 
Code_Sub2::makeDFA()
{
  dfa = dfaSubset(offsets.off(id1), offsets.off(id2));
}

void 
Code_Sub2::makeGTA()
{
  gta = gtaSub(offsets.off(id1), offsets.off(id2),
	       stateSpaces(id1), stateSpaces(id2));
}

void 
Code_Less1::makeDFA()
{
  dfa = dfaLess(offsets.off(id1), offsets.off(id2));
}

void 
Code_Less1::makeGTA()
{
  gta = gtaLess(offsets.off(id1), offsets.off(id2), 
		stateSpaces(id1), stateSpaces(id2));
}

void 
Code_LessEq1::makeDFA()
{
  dfa = dfaLesseq(offsets.off(id1), offsets.off(id2));
}


void 
Code_LessEq1::makeGTA()
{
  gta = gtaLesseq(offsets.off(id1), offsets.off(id2), 
		  stateSpaces(id1), stateSpaces(id2));
}

void 
Code_EqDot0::makeGTA()
{
  gta = gtaDot0(offsets.off(id1), offsets.off(id2), 
		stateSpaces(id1), stateSpaces(id2));
}

void 
Code_EqDot1::makeGTA()
{
  gta = gtaDot1(offsets.off(id1), offsets.off(id2), 
		stateSpaces(id1), stateSpaces(id2));
}

void 
Code_EqUp::makeGTA()
{
  gta = gtaUp(offsets.off(id1), offsets.off(id2), 
	      stateSpaces(id1), stateSpaces(id2));
}

void 
Code_EqPlus2::makeDFA()
{
  dfa = dfaPlus2(offsets.off(id1), offsets.off(id2));
}

void 
Code_EqMinus2::makeDFA()
{
  dfa = dfaMinus2(offsets.off(id1), offsets.off(id2));
}

void 
Code_EqMin::makeDFA()
{
  dfa = dfaMin(offsets.off(id1), offsets.off(id2));
}

void 
Code_EqMax::makeDFA()
{
  dfa = dfaMax(offsets.off(id1), offsets.off(id2));
}

void 
Code_EqPlus1::makeDFA()
{
  dfa = dfaPlus1(offsets.off(id1), offsets.off(id2), val);
}

void 
Code_EqMinus1::makeDFA()
{
  dfa = dfaMinus1(offsets.off(id1), offsets.off(id2));
}

void 
Code_EqUnion::makeDFA()
{
  dfa = dfaUnion(offsets.off(id1), offsets.off(id2), offsets.off(id3));
}

void 
Code_EqUnion::makeGTA()
{
  gta = gtaUnion(offsets.off(id1), offsets.off(id2), offsets.off(id3),
		 stateSpaces(id1));
}

void 
Code_EqInter::makeDFA()
{
  dfa = dfaInter(offsets.off(id1), offsets.off(id2), offsets.off(id3));
}

void 
Code_EqInter::makeGTA()
{
  gta = gtaInter(offsets.off(id1), offsets.off(id2), offsets.off(id3),
		 stateSpaces(id1));
}

void
Code_EqSetMinus::makeDFA()
{
  dfa = dfaSetminus(offsets.off(id1), offsets.off(id2), offsets.off(id3));
}

void 
Code_EqSetMinus::makeGTA()
{  
  gta = gtaSetminus(offsets.off(id1), offsets.off(id2), offsets.off(id3),
		    stateSpaces(id1));
}

void
Code_EqPlusModulo::makeDFA()
{
  dfa = dfaPlusModulo1(offsets.off(id1), offsets.off(id2), offsets.off(id3));
}

void
Code_EqMinusModulo::makeDFA()
{
  dfa = dfaMinusModulo1(offsets.off(id1), offsets.off(id2), offsets.off(id3));
}

void
Code_EqPresbConst::makeDFA()
{
  dfa = dfaPresbConst(offsets.off(id), val);
}

void
Code_WellFormedTree::makeGTA()
{
  gta = gtaWellFormedTree(offsets.off(id), stateSpaces(id));
}

////////// Code_Restrict //////////////////////////////////////////////////////

void 
Code_Restrict::makeDFA()
{
  dfa = st_dfa_restrict(vc.DFATranslate(), pos);
  vc.remove();
}

void 
Code_Restrict::makeGTA()
{
  gta = st_gta_restrict(vc.GTATranslate(), pos);
  vc.remove();
}

////////// Code_Project ///////////////////////////////////////////////////////

Code_Project::Code_Project(Ident n, VarCode c, Pos p) :
  Code_c(cProject, c, p, true), var(n)
{
  IdentList::iterator i;

  varpos = 0;
  for (i = vc.vars->begin(); i != vc.vars->end(); i++) {
    if (offsets.off(*i) < offsets.off(var))
      varpos++; // find relative offset of var
    if (*i != var)
      vars.push_back(*i);
  }
  sign.make(vars);
  vars.compress();
}

bool
Code_Project::equiv(Code &c)
{
  return kind == c.kind && 
    varpos == ((Code_Project&) c).varpos && // same relative var offset pos.
    vc.code == ((Code_Project&) c).vc.code &&
    sign == ((Code_Project&) c).sign &&
    sameUnivs(var, ((Code_Project&) c).var);
}

void 
Code_Project::makeDFA()
{
  dfa = st_dfa_minimization(st_dfa_project
			    (vc.DFATranslate(), 
			     var, pos));
  vc.remove();
}

void 
Code_Project::makeGTA()
{
  gta = st_gta_minimization(st_gta_project
			    (vc.GTATranslate(),
			     var, pos));
  vc.remove();
}

////////// Code_Negate ////////////////////////////////////////////////////////

void 
Code_Negate::makeDFA()
{
  dfa = st_dfa_negation(vc.DFATranslate(), pos);
  vc.remove();
}

void 
Code_Negate::makeGTA()
{
  gta = st_gta_negation(vc.GTATranslate(), pos);
  vc.remove();
}

////////// Code_Prefix ////////////////////////////////////////////////////////

void 
Code_Prefix::makeDFA()
{
  dfa = st_dfa_minimization(st_dfa_prefix(vc.DFATranslate(), pos));
  vc.remove();
}

////////// Code_And ///////////////////////////////////////////////////////////

void 
Code_And::makeDFA()
{
  DFA *a1 = vc1.DFATranslate();
  vc1.remove();
  DFA *a2 = vc2.DFATranslate();
  vc2.remove();
  dfa = st_dfa_minimization(st_dfa_product(a1, a2, dfaAND, pos));
}

void 
Code_And::makeGTA()
{
  GTA *g1 = vc1.GTATranslate();
  vc1.remove();
  GTA *g2 = vc2.GTATranslate();
  vc2.remove();
  gta = st_gta_minimization(st_gta_product(g1, g2, gtaAND, pos));
}

////////// Code_IdLeft ////////////////////////////////////////////////////////

void 
Code_IdLeft::makeDFA()
{
  DFA *a1 = vc1.DFATranslate();
  vc1.remove();
  DFA *a2 = vc2.DFATranslate();
  vc2.remove();
  dfaFree(a2);
  dfa = a1;
}

void 
Code_IdLeft::makeGTA()
{
  GTA *g1 = vc1.GTATranslate();
  vc1.remove();
  GTA *g2 = vc2.GTATranslate();
  vc2.remove();
  gtaFree(g2);
  gta = g1;
}

////////// Code_Or ////////////////////////////////////////////////////////////

void 
Code_Or::makeDFA()
{
  DFA *a1 = vc1.DFATranslate();
  vc1.remove();
  DFA *a2 = vc2.DFATranslate();
  vc2.remove();
  dfa = st_dfa_minimization(st_dfa_product(a1, a2, dfaOR, pos));
}

void 
Code_Or::makeGTA()
{
  GTA *g1 = vc1.GTATranslate();
  vc1.remove();
  GTA *g2 = vc2.GTATranslate();
  vc2.remove();
  gta = st_gta_minimization(st_gta_product(g1, g2, gtaOR, pos));
}

////////// Code_Impl //////////////////////////////////////////////////////////

void 
Code_Impl::makeDFA()
{
  DFA *a1 = vc1.DFATranslate();
  vc1.remove();
  DFA *a2 = vc2.DFATranslate();
  vc2.remove();
  dfa = st_dfa_minimization(st_dfa_product(a1, a2, dfaIMPL, pos));
}

void 
Code_Impl::makeGTA()
{
  GTA *g1 = vc1.GTATranslate();
  vc1.remove();
  GTA *g2 = vc2.GTATranslate();
  vc2.remove();
  gta = st_gta_minimization(st_gta_product(g1, g2, gtaIMPL, pos));
}

////////// Code_Biimpl ////////////////////////////////////////////////////////

void 
Code_Biimpl::makeDFA()
{
  DFA *a1 = vc1.DFATranslate();
  vc1.remove();
  DFA *a2 = vc2.DFATranslate();
  vc2.remove();
  dfa = st_dfa_minimization(st_dfa_product(a1, a2, dfaBIIMPL, pos));
}

void 
Code_Biimpl::makeGTA()
{
  GTA *g1 = vc1.GTATranslate();
  vc1.remove();
  GTA *g2 = vc2.GTATranslate();
  vc2.remove();
  gta = st_gta_minimization(st_gta_product(g1, g2, gtaBIIMPL, pos));
}

////////// Code_PredCall //////////////////////////////////////////////////////

Code_PredCall::Code_PredCall(Ident n, VarCode c, IdentList &acts, 
			     IdentList *frs, char *source, Pos p) :
  Code_c(cPredCall, c, p, true), name(n), sourcefile(source)
{
  actuals.append(&acts);
  frees.append(frs);

  // predcalls are always using signature equivalence
  // based on the actuals+frees,
  // all occuring variables must be in the substitution list
  vars.append(&acts);
  vars.append(frs);
  sign.make(vars);
  vars.compress(); // note: all vars might not occur!

  // make shadow of vars
  for (Ident i = 0; (unsigned) i < vars.size(); i++)
    s.push_back(i);

  // get names and orders
  unsigned i;
  names = new char*[vars.size()];
  orders = new char[vars.size()];
  for (i = 0; i < vars.size(); i++) {
    names[i] = symbolTable.lookupSymbol(vars.get(i));
    orders[i] = (char) symbolTable.lookupOrder(vars.get(i));
  }

  // get separate compilation file name
  if (options.separateCompilation) {
    Deque<SSSet> statespaces;
    IdentList::iterator i;
    for (i = vars.begin(); i != vars.end(); i++) 
      statespaces.push_back(stateSpaces(*i));
    filename = lib.getFileName(symbolTable.lookupSymbol(name),
			       sourcefile, &sign, &statespaces);
    Deque<SSSet>::iterator j;
    for (j = statespaces.begin(); j != statespaces.end(); j++) 
      mem_free(*j);
  }
  else
    filename = NULL;
}

bool
Code_PredCall::equiv(Code &c)
{
  if (!(kind == c.kind &&
	name == ((Code_PredCall&) c).name &&
	sign == ((Code_PredCall&) c).sign &&
	vars.size() == ((Code_PredCall&) c).vars.size()))
    return false;
  for (IdentList::iterator i = vars.begin(),
	 j = ((Code_PredCall&) c).vars.begin();
       i != vars.end(); i++, j++)
    if (!sameUnivs(*i, *j)) // check variable state spaces
      return false;
  return true;
}

unsigned
Code_PredCall::hash()
{
  return kind + sign.hashvalue*3 + name*128;
}

void 
Code_PredCall::makeDFA()
{
  if (options.separateCompilation && lib.fileExists(filename)) {
    // found on disk
    if (options.statistics)
      cout << "-- Importing '" << filename << "' --\n";

    dfa = dfaImport(filename, NULL, NULL);
    if (!dfa)
      error((String) "Error reading file '" + filename + "'");
    st_dfa_replace_indices(dfa, &vars, &s, true, false); 
  }

  else { // need to make automaton
    if (options.statistics)
      cout << "-- Entering predicate '" << symbolTable.lookupSymbol(name)
	   << "' --\n";   

    dfa = vc.DFATranslate();
    st_dfa_replace_indices(dfa, &s, &vars, false, true);

    if (options.separateCompilation) {
      if (options.statistics)
	cout << "-- Exporting '" << filename << "' --\n";
      if (!dfaExport(dfa, filename, vars.size(), names, orders)) 
	error("Unable to write file");
    }

    st_dfa_replace_indices(dfa, &vars, &s, true, false);

    if (options.statistics)
      cout << "-- Leaving predicate '" << symbolTable.lookupSymbol(name)
	   << "' (states: " << dfa->ns << ") --\n";
  }
  vc.remove();
}

void 
Code_PredCall::makeGTA()
{
  if (options.separateCompilation && lib.fileExists(filename)) {
    // found on disk
    if (options.statistics)
      cout << "-- Importing '" << filename << "' --\n";
    gta = gtaImport(filename, NULL, NULL, NULL, false);
    if (!gta)
      error((String) "Error reading file '" + filename + "'");
    st_gta_replace_indices(gta, &vars, &s, true, false); 
  }

  else { // need to make automaton
    if (options.statistics)
      cout << "-- Entering predicate '" << symbolTable.lookupSymbol(name)
	   << "' --\n";

    gta = vc.GTATranslate();
    st_gta_replace_indices(gta, &s, &vars, false, true);

    if (options.separateCompilation) {
      SSSet *statespaces = new SSSet[vars.size()];
      for (unsigned i = 0; i < vars.size(); i++)
	statespaces[i] = stateSpaces(vars.get(i));
      if (options.statistics)
	cout << "-- Exporting '" << filename << "' --\n";
      if (!gtaExport(gta, filename, vars.size(), names, 
		     orders, statespaces, false)) 
	error("Unable to write file");
      for (unsigned i = 0; i < vars.size(); i++)
	if (statespaces[i]) {
	  mem_free(statespaces[i]);
	  statespaces[i] = 0;
	}
      delete[] statespaces;
    }

    st_gta_replace_indices(gta, &vars, &s, true, false);

    if (options.statistics) {
      unsigned i, n = 0;
      for (i = 0; i < guide.numSs; i++)
	n += gta->ss[i].size;
      cout << "-- Leaving predicate '" << symbolTable.lookupSymbol(name)
	   << "' (states: " << n << ") --\n";
    }
  }
  vc.remove();
}

////////// Code_Import ////////////////////////////////////////////////////////

Code_Import::Code_Import(char *filename, Deque<char*> *forms, 
			 IdentList *acts, Pos p) :
  Code(cImport, p), file(filename), formals(forms), actuals(acts) 
{
  vars.insert(actuals);
}

bool
Code_Import::equiv(Code &c)
{
  return &c == this; // always fresh node
}

unsigned
Code_Import::hash()
{
  return kind + (int) file;
}

void
Code_Import::makeDFA()
{
  if (options.statistics)
    cout << "-- Importing '" << file << "' --\n";

  char **fileVars; // null terminated array of names in index order
  int *fileOrders; // corresponding array of orders

  dfa = dfaImport(file, &fileVars, &fileOrders);
  if (!dfa) 
    error((String) "Error reading file '" + file + "'");

  IdentList *off = getOffsets(fileVars, fileOrders, NULL);
  st_dfa_replace_indices(dfa, actuals, off, true, false); 

  for (int i = 0; fileVars[i]; i++)
    mem_free(fileVars[i]);
  mem_free(fileVars);
  mem_free(fileOrders);
  delete off;
}

void
Code_Import::makeGTA()
{
  if (options.statistics)
    cout << "-- Importing '" << file << "' --\n";

  char **fileVars; // null terminated array of names in index order
  int *fileOrders; // corresponding array of orders
  SSSet *fileSS; // corresponding array of state-space sets

  gta = gtaImport(file, &fileVars, &fileOrders, &fileSS, false);
  if (!gta)
    error((String) "Error reading file '" + file + "'");

  IdentList *off = getOffsets(fileVars, fileOrders, fileSS);
  st_gta_replace_indices(gta, actuals, off, true, false); 

  for (int i = 0; fileVars[i]; i++) {
    mem_free(fileVars[i]);
    mem_free(fileSS[i]);
  }
  mem_free(fileVars);
  mem_free(fileOrders);
  mem_free(fileSS);
  delete off;
}

IdentList *
Code_Import::getOffsets(char **fileVars, int *fileOrders, SSSet *fileSS)
{
  // make offset array
  IdentList *off = new IdentList; // really an offset list, but who cares
  int i = 0;
  while (fileVars[i] != 0)
    i++;
  if ((unsigned) i != actuals->size())
    error("Wrong number of parameters");
  IdentList::iterator j;
  Deque<char*>::iterator k;
  for (j = actuals->begin(), k = formals->begin(); 
       j != actuals->end(); j++, k++) {
    for (i = 0; fileVars[i] != 0; i++)
      if (strcmp(fileVars[i], *k) == 0) {
	off->push_back(i);
	if (symbolTable.lookupOrder(*j) != fileOrders[i])
	  error("Orders of variables do not match");
	if (fileSS) {
	  SSSet ss = stateSpaces(*j);
	  if (memcmp(ss, fileSS[i], guide.numSs))
	    error("Variable state-spaces do not match");
	  mem_free(ss);
	}
	break;
      }
    if (fileVars[i] == 0)
      error((String) "Formal '" + *k + "' not found in file");
  }

  // check signature
  Signature s(*actuals);
  for (i = 0; (unsigned) i < s.size; i++)
    if (s.sign[i] != off->get(i))
      error("Variable ordering does not match");
  
  return off;
}

////////// Code_Export ////////////////////////////////////////////////////////

Code_Export::Code_Export(VarCode c, char *fileName, IdentList *fv, Pos p) :
  Code_c(cExport, c, p), file(fileName)
{
  // find free vars, names, and orders
  unsigned i, j;
  names = new char*[fv->size()];
  orders = new char[fv->size()];
  for (i = 0, j = 0; i < fv->size(); i++) {
    Ident id = fv->get(i);
    if (id != lastPosVar && id != allPosVar) {
      freevars.push_back(id); // free vars
      s.push_back(j); // shadow of free vars
      names[j] = symbolTable.lookupSymbol(id);
      orders[j] = (char) symbolTable.lookupOrder(id);
      j++;
    }
  }
  num = j;
}

bool
Code_Export::equiv(Code &c)
{
  return &c == this; // always fresh node
}

unsigned
Code_Export::hash()
{
  return kind + (int) file;
}

void 
Code_Export::makeDFA()
{
  dfa = vc.DFATranslate();
  
  if (options.statistics)
    cout << "-- Exporting '" << file << "' --\n";
  
  DFA *dfa2 = dfaCopy(dfa);
  if (lastPosVar != -1)
    dfa2 = st_dfa_lastpos(dfa2, offsets.off(lastPosVar));
  if (allPosVar != -1)
    dfa2 = st_dfa_allpos(dfa2, offsets.off(allPosVar));
  if (options.unrestrict) {
    dfaUnrestrict(dfa2);
    DFA *t = dfa2;
    dfa2 = dfaMinimize(dfa2);
    dfaFree(t);
  }
  st_dfa_replace_indices(dfa2, &s, &freevars, false, true);
  if (!dfaExport(dfa2, file, num, names, orders))
    error("Unable to write file");
  dfaFree(dfa2);
  
  vc.remove();
}

void 
Code_Export::makeGTA()
{
  gta = vc.GTATranslate();
  
  if (options.statistics)
    cout << "-- Exporting '" << file << "' --\n";
  
  SSSet *statespaces = new SSSet[freevars.size()];
  for (unsigned i = 0; i < freevars.size(); i++)
    statespaces[i] = stateSpaces(freevars.get(i));

  GTA *gta2 = gtaCopy(gta);
  if (allPosVar != -1)
    gta2 = st_gta_allpos(gta2, offsets.off(allPosVar));
  if (options.unrestrict) {
    gtaUnrestrict(gta2);
    GTA *t = gta2;
    gta2 = gtaMinimize(gta2);
    gtaFree(t);
  }
  st_gta_replace_indices(gta2, &s, &freevars, false, true);
  if (!gtaExport(gta2, file, num, names, orders, statespaces, 
		 options.inheritedAcceptance)) 
    error("Unable to write file");
  gtaFree(gta2);

  for (unsigned i = 0; i < freevars.size(); i++)
    mem_free(statespaces[i]);
  delete[] statespaces;
  
  vc.remove();
}
