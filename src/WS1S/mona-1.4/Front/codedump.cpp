
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
#include "code.h"
#include "offsets.h"
#include "symboltable.h"
#include "env.h"

extern "C" {
#include "../Mem/mem.h"
}

extern Options options;
extern SymbolTable symbolTable;

////////// stateSpaces ////////////////////////////////////////////////////////

void
dumpStateSpaces(SSSet set)
{
  unsigned i, j = 0;
  cout << "[";
  for (i = 0; i < guide.numSs; i++)
    if (set[i])
      j++;
  if (j > 5)
    cout << "...";
  else {
    for (i = 0; i < guide.numSs; i++)
      if (set[i]) {
	cout << i;
	break;
      }
    for (i++; i < guide.numSs; i++)
      if (set[i])
	cout << "," << i;
  }
  cout << "]";
  mem_free(set);
}

void
dumpStateSpaces(Ident id)
{
  if (options.mode != TREE)
    return;

  dumpStateSpaces(stateSpaces(id));
}

/////////// VarCode ///////////////////////////////////////////////////////////

void 
VarCode::dump(bool rec)
{
  code->dump(rec);
  dumpsubst();
}

void 
VarCode::dumpsubst()
{
  IdentList::iterator i,j;
  bool first = true, any = false;
  for (i = code->vars.begin(), j = vars->begin();
       i != code->vars.end(); i++, j++)
    if (*i != *j) 
      any = true;
  if (any) {
    cout << "[";
    for (i = code->vars.begin(), j = vars->begin();
	 i != code->vars.end(); i++, j++)
      if  (*i != *j) {
	if (!first)
	  cout << ",";
	cout << "#" << *i << "->#" << *j;
	first = false;
      }
    cout << "]";
  }
}

void
VarCodeList::dump()
{
  cout << "[";
  for (VarCodeList::iterator i = begin(); i != end(); i++) {
    if (i != begin())
      cout << ",";
    printf("(%x,", (unsigned) (*i).code);
    if ((*i).vars)
      (*i).vars->dumplist();
    printf(")");
  }
  cout << "]";
}

/////////// Code //////////////////////////////////////////////////////////////

void 
Code::viz()
{
  if (!mark) {
    printf(" L%x [label=\"", (unsigned) this);
    dump(false);
    printf("\"];\n");
    mark = true;
  }
}

void
Code_c::viz()
{
  if (!mark) {
    vc.code->viz();
    Code::viz();
    printf(" L%x -> L%x [label=\"", (unsigned) this, (unsigned) vc.code);
    vc.dumpsubst();
    printf("\"];\n");
    mark = true;
  }
}

void
Code_cc::viz()
{
  if (!mark) {
    vc1.code->viz();
    vc2.code->viz();
    Code::viz();
    printf(" L%x -> L%x [label=\"", (unsigned) this, (unsigned) vc1.code);
    vc1.dumpsubst();
    printf("\"];\n");
    printf(" L%x -> L%x [label=\"", (unsigned) this, (unsigned) vc2.code);
    vc2.dumpsubst();
    printf("\"];\n");
    mark = true;
  }
}

void
Code::show()
{
  dump(false);
  cout << "\n";
}

/////////// Code leaves ///////////////////////////////////////////////////////

void
Code_True::dump(bool rec)
{
  cout << "True()";
}

void
Code_False::dump(bool rec)
{
  cout << "False()";
}

void
Code_EqEmpty::dump(bool rec)
{
  cout << "Empty(#" << id << ")";
}

void
Code_EqRoot::dump(bool rec)
{
  cout << "Root(#" << id << ",";
  universes->dump();
  cout << ")";
}

void
Code_FirstOrder::dump(bool rec)
{
  cout << "FirstOrder(#" << id << ")";
}

void
Code_EqConst::dump(bool rec)
{
  cout << "Const(#" << id << "," << val << ")";
}

void
Code_Singleton::dump(bool rec)
{
  cout << "Singleton(#" << id << ")";
}

void
Code_BoolVar::dump(bool rec)
{
  cout << "BoolVar(#" << id << ")";
}

void
Code_InStateSpace::dump(bool rec)
{
  cout << "InStateSpace(#" << id << ",[";
  if (ss && ss->size() > 5)
    cout << "...";
  else {
    IdentList::iterator i;
    for (i = ss->begin(); i != ss->end(); i++) {
      if (i != ss->begin())
	cout << ",";
      cout << symbolTable.lookupNumber(*i);
    }
  }
  cout << "])";
}

void
Code_SomeType::dump(bool rec)
{
  cout << "SomeType(#" << id << ")";
}

void
Code_In::dump(bool rec)
{
  cout << "In(#" << id1 << ",#" << id2 << ")";
}

void
Code_Eq1::dump(bool rec)
{
  cout << "Eq1(#" << id1 << ",#" << id2 << ")";
}

void
Code_Eq2::dump(bool rec)
{
  cout << "Eq2(#" << id1 << ",#" << id2 << ")";
}

void
Code_Sub2::dump(bool rec)
{
  cout << "Sub2(#" << id1 << ",#" << id2 << ")";
}

void
Code_Less1::dump(bool rec)
{
  cout << "Less1(#" << id1 << ",#" << id2 << ")";
}

void
Code_LessEq1::dump(bool rec)
{
  cout << "LessEq1(#" << id1 << ",#" << id2 << ")";
}

void
Code_EqDot0::dump(bool rec)
{
  cout << "Dot0(#" << id1 << ",#" << id2 << ")";
}

void
Code_EqDot1::dump(bool rec)
{
  cout << "Dot1(#" << id1 << ",#" << id2 << ")";
}

void
Code_EqUp::dump(bool rec)
{
  cout << "eqUp(#" << id1 << ",#" << id2 << ")";
}

void
Code_EqPlus2::dump(bool rec)
{
  cout << "EqPlus2(#" << id1 << ",#" << id2 << ")";
}

void
Code_EqMinus2::dump(bool rec)
{
  cout << "EqMinus2(#" << id1 << ",#" << id2 << ")";
}

void
Code_EqMin::dump(bool rec)
{
  cout << "EqMin(#" << id1 << ",#" << id2 << ")";
}

void
Code_EqMax::dump(bool rec)
{
  cout << "EqMax(#" << id1 << ",#" << id2 << ")";
}

void
Code_EqPlus1::dump(bool rec)
{
  cout << "EqPlus1(#" << id1 << ",#" << id2 << "," << val << ")";
}

void
Code_EqMinus1::dump(bool rec)
{
  cout << "EqMinus1(#" << id1 << ",#" << id2 << ")";
}

void
Code_EqUnion::dump(bool rec)
{
  cout << "Union(#" << id1 << ",#" << id2 << ",#" << id3 << ")";
}

void
Code_EqInter::dump(bool rec)
{
  cout << "Inter(#" << id1 << ",#" << id2 << ",#" << id3 << ")";
}

void
Code_EqSetMinus::dump(bool rec)
{
  cout << "SetMinus(#" << id1 << ",#" << id2 << ",#" << id3 << ")";
}

void
Code_EqPlusModulo::dump(bool rec)
{
  cout << "EqPlusModulo(#" << id1 << ",#" << id2 << ",#" << id3 << ")";
}

void
Code_EqMinusModulo::dump(bool rec)
{
  cout << "EqMinusModulo(#" << id1 << ",#" << id2 << ",#" << id3 << ")";
}

void
Code_EqPresbConst::dump(bool rec)
{
  cout << "PresbConst(#" << id << "," << val << ")";
}

void
Code_WellFormedTree::dump(bool rec)
{
  cout << "WellFormedTree(#" << id << ")";
}

/////////// Code composite ////////////////////////////////////////////////////

void
Code_Restrict::dump(bool rec)
{
  cout << "Restrict";
  if (rec) {
    cout << "(";
    vc.dump(rec);
    cout << ")";
  }
}

void
Code_Project::dump(bool rec)
{
  cout << "Project(#" << var;
  dumpStateSpaces(var);
  if (rec) {
    cout << ",";
    vc.dump(rec);
  }
  cout << ")";
}

void
Code_Negate::dump(bool rec)
{
  cout << "Negate";
  if (rec) {
    cout << "(";
    vc.dump(rec);
    cout << ")";
  }
}

void
Code_Prefix::dump(bool rec)
{
  cout << "Prefix";
  if (rec) {
    cout << "(";
    vc.dump(rec);
    cout << ")";
  }
}

void
Code_And::dump(bool rec)
{
  cout << "And";
  if (rec) {
    cout << "(";
    vc1.dump(rec);
    cout << ",";
    vc2.dump(rec);
    cout << ")";
  }
}

void
Code_IdLeft::dump(bool rec)
{
  cout << "IdLeft";
  if (rec) {
    cout << "(";
    vc1.dump(rec);
    cout << ",";
    vc2.dump(rec);
    cout << ")";
  }
}

void
Code_Or::dump(bool rec)
{
  cout << "Or";
  if (rec) {
    cout << "(";
    vc1.dump(rec);
    cout << ",";
    vc2.dump(rec);
    cout << ")";
  }
}

void
Code_Impl::dump(bool rec)
{
  cout << "Impl";
  if (rec) {
    cout << "(";
    vc1.dump(rec);
    cout << ",";
    vc2.dump(rec);
    cout << ")";
  }
}

void
Code_Biimpl::dump(bool rec)
{
  cout << "Biimpl";
  if (rec) {
    cout << "(";
    vc1.dump(rec);
    cout << ",";
    vc2.dump(rec);
    cout << ")";
  }
}

void
Code_PredCall::dump(bool rec)
{
  cout << "PredCall(" << symbolTable.lookupSymbol(name);
  if (rec) {
    cout << ",";
    vc.dump(rec);
  }
  cout << ")";
}

void
Code_Import::dump(bool rec)
{
  cout << "Import(" << file;
  Deque<char*>::iterator i;
  IdentList::iterator j;
  for (i = formals->begin(), j = actuals->begin();
       i != formals->end(); i++, j++)
    cout << "," << *i << "->#" << *j;
  cout << ")";
}

void
Code_Export::dump(bool rec)
{
  cout << "Export(" << file;
  if (rec) {
    cout << ",";
    vc.dump(rec);
  }
  cout << ")";
}
