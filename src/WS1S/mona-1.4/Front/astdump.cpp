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

#include "ast.h"
#include "symboltable.h"

extern SymbolTable symbolTable;

void 
ASTComponent::dump()
{
  cout << name;
  if (path)
    path->dump();
  else
    cout << "[?]";
  cout << ":" << type;
}

void
ASTComponentList::dump()
{
  for (iterator i = begin(); i != end(); i++) {
    if (i != begin())
      cout << ",";
    (*i)->dump();
  }
}

void
ASTVariant::dump()
{
  cout << name;
  if (path)
    path->dump();
  else
    cout << "[?]";
  cout << "(";
  if (components)
    components->dump();
  cout << ")";
}

void
ASTVariantList::dump()
{
  for (iterator i = begin(); i != end(); i++) {
    if (i != begin())
      cout << ",";
    (*i)->dump();
  }
}

void
BitList::dump()
{
  cout << "[";
  for (iterator i = begin(); i != end(); i++)
    if (*i == Zero)
      cout << "0";
    else
      cout << "1";
  cout << "]";
}

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
ASTTerm1_Var1::dump()
{
  cout << "Var1 " << symbolTable.lookupSymbol(n);
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
    
void 
ASTTerm1_Up::dump()
{
  cout << "Up("; t->dump(); cout << ")";
}
    
void
ASTTerm1_Root::dump()
{
  if (univ == -1) {
    IdentList *univs = symbolTable.allUnivs();
    univ = univs->get(0);
    delete univs;
  }
  cout << "Root(" << symbolTable.lookupSymbol(univ) << ")";
}

void 
ASTTerm1_Int::dump()
{
  cout << "Int " << n;
}
    
void 
ASTTerm1_Plus::dump()
{
  cout << "Plus1("; t->dump(); cout << "," << n << ")";
}
    
void 
ASTTerm1_Minus::dump()
{
  cout << "Minus1("; t->dump(); cout << "," << n << ")";
}
    
void 
ASTTerm1_PlusModulo::dump()
{
  cout << "PlusModulo1("; t1->dump(); cout << ","  << n << ",";
  t2->dump(); cout << ")";
}
    
void 
ASTTerm1_MinusModulo::dump()
{
  cout << "MinusModulo1("; t1->dump(); cout << "," << n << ",";
  t2->dump(); cout << ")";
}
    
void 
ASTTerm1_Min::dump()
{
  cout << "Min("; T->dump(); cout << ")";
}
    
void 
ASTTerm1_Max::dump()
{
  cout << "Max("; T->dump(); cout << ")";
}
    
void 
ASTTerm1_TreeRoot::dump()
{
  cout << "TreeRoot("; T->dump(); cout << ")";
}
    
void 
ASTTerm2_Var2::dump()
{
  cout << "Var2 " << symbolTable.lookupSymbol(n);
}
    
void 
ASTTerm2_VarTree::dump()
{
  cout << "Tree " << symbolTable.lookupSymbol(n);
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
    
void 
ASTTerm2_Up::dump()
{
  cout << "Up("; T->dump(); cout << ")";
}
    
void 
ASTTerm2_Empty::dump()
{
  cout << "Empty";
}
    
void 
ASTTerm2_Union::dump()
{
  cout << "Union("; T1->dump(); cout << ","; T2->dump(); cout << ")";
}
    
void 
ASTTerm2_Inter::dump()
{
  cout << "Inter("; T1->dump(); cout << ","; T2->dump(); cout << ")";
}
    
void 
ASTTerm2_Setminus::dump()
{
  cout << "Setminus("; T1->dump(); cout << ","; T2->dump(); cout << ")";
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
    
void 
ASTTerm2_Plus::dump()
{
  cout << "Plus2("; T->dump(); cout << "," << n << ")";
}
    
void 
ASTTerm2_Minus::dump()
{
  cout << "Minus2("; T->dump(); cout << "," << n << ")";
}
    
void 
ASTTerm2_Interval::dump()
{
  cout << "Interval("; t1->dump(); cout << ","; t2->dump(); cout << ")";
}
    
void 
ASTTerm2_PresbConst::dump()
{
  cout << "PresbConst(" << value << ")"; 
}

void 
ASTTerm2_Formula::dump()
{
  cout << "Term2Formula(" << symbolTable.lookupSymbol(fresh) << ",";
  f->dump(); cout << ")";
}

void 
ASTForm_Var0::dump()
{
  cout << "Var0 " << symbolTable.lookupSymbol(n);
}
    
void 
ASTForm_True::dump()
{
  cout << "True";
}
    
void 
ASTForm_False::dump()
{
  cout << "False";
}
    
void 
ASTForm_In::dump()
{
  cout << "In("; t1->dump(); cout << ","; T2->dump(); cout << ")";
}
    
void 
ASTForm_Notin::dump()
{
  cout << "Notin("; t1->dump(); cout << ","; T2->dump(); cout << ")";
}
    
void
ASTForm_RootPred::dump()
{
  cout << "Root("; t->dump(); cout << ",["; ul->dump(); cout << ")"; 
}

void 
ASTForm_EmptyPred::dump()
{
  cout << "EmptyPred("; T->dump(); cout << ")";
}
    
void 
ASTForm_FirstOrder::dump()
{
  cout << "FirstOrder("; t->dump(); cout << ")";
}
    
void 
ASTForm_Sub::dump()
{
  cout << "Sub("; T1->dump(); cout << ","; T2->dump(); cout << ")";
}
    
void 
ASTForm_Equal1::dump()
{
  cout << "Equal1("; t1->dump(); cout << ","; t2->dump(); cout << ")";
}
    
void 
ASTForm_Equal2::dump()
{
  cout << "Equal2("; T1->dump(); cout << ","; T2->dump(); cout << ")";
}
    
void 
ASTForm_NotEqual1::dump()
{
  cout << "NotEqual1("; t1->dump(); cout << ","; t2->dump(); cout << ")";
}
    
void 
ASTForm_NotEqual2::dump()
{
  cout << "NotEqual2("; T1->dump(); cout << ","; T2->dump(); cout << ")";
}
    
void 
ASTForm_Less::dump()
{
  cout << "Less("; t1->dump(); cout << ","; t2->dump(); cout << ")";
}
    
void 
ASTForm_LessEq::dump()
{
  cout << "LessEq("; t1->dump(); cout << ","; t2->dump(); cout << ")";
}
    
void 
ASTForm_WellFormedTree::dump()
{
  cout << "WellFormedTree("; T->dump(); cout << ")";
}
    
void 
ASTForm_Impl::dump()
{
  cout << "Impl("; f1->dump(); cout << ","; f2->dump(); cout << ")";
}
    
void 
ASTForm_Biimpl::dump()
{
  cout << "Biimpl("; f1->dump(); cout << ","; f2->dump(); cout << ")";
}
    
void 
ASTForm_And::dump()
{
  cout << "And("; f1->dump(); cout << ","; f2->dump(); cout << ")";
}
    
void 
ASTForm_IdLeft::dump()
{
  cout << "IdLeft("; f1->dump(); cout << ","; f2->dump(); cout << ")";
}
    
void 
ASTForm_Or::dump()
{
  cout << "Or("; f1->dump(); cout << ","; f2->dump(); cout << ")";
}
    
void 
ASTForm_Not::dump()
{
  cout << "Not("; f->dump(); cout << ")";
}
    
void 
ASTForm_Ex0::dump()
{
  cout << "Ex0("; vl->dump(); cout << ","; f->dump(); cout << ")";
}
    
void 
ASTForm_Ex1::dump()
{
  cout << "Ex1("; vl->dump(); cout << ","; f->dump(); cout << ")";
}
    
void 
ASTForm_Ex2::dump()
{
  cout << "Ex2("; vl->dump(); cout << ","; f->dump(); cout << ")";
}
    
void 
ASTForm_All0::dump()
{
  cout << "All0("; vl->dump(); cout << ","; f->dump(); cout << ")";
}
    
void 
ASTForm_All1::dump()
{
  cout << "All1("; vl->dump(); cout << ","; f->dump(); cout << ")";
}
    
void 
ASTForm_All2::dump()
{
  cout << "All2("; vl->dump(); cout << ","; f->dump(); cout << ")";
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
    
void 
ASTForm_Call::dump()
{
  cout << "Call(" << symbolTable.lookupSymbol(n);
  args->dump();
  cout << ")";
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

void 
ASTForm_Export::dump()
{
  cout << "Export(\"" << file << "\","; f->dump(); cout << ")";
}

void 
ASTForm_Prefix::dump()
{
  cout << "Prefix("; f->dump(); cout << ")";
}

void 
ASTForm_Restrict::dump()
{
  cout << "Restrict("; f->dump(); cout << ")";
}

void 
ASTForm_InStateSpace1::dump()
{
  cout << "InStateSpace1("; t->dump(); 
  cout << ","; ss->dump(); cout << ")";
}

void 
ASTForm_InStateSpace2::dump()
{
  cout << "InStateSpace2("; T->dump(); 
  cout << ","; ss->dump(); cout << ")";
}

void 
ASTForm_SomeType::dump()
{
  cout << "SomeType("; t->dump(); cout << ")";
}
