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

extern CodeTable *codeTable;
extern bool regenerate;

VarCode
VarCode::substCopy(IdentList *formals, IdentList *actuals)
{ // make copy with actuals substituting formals
  VarCode res;
  IdentList *newactuals = subst(vars, formals, actuals);
  if (!regenerate && equal(vars, newactuals)) 
    // nothing to substitute, just copy
    res = VarCode(newactuals, code);
  else { 
    // see if substitution already has been made
    Signature sign(*newactuals);
    Deque<SubstCopy>::iterator i;
    for (i = code->sclist.begin(); i != code->sclist.end(); i++)
      if (sign == *(*i).sign && sameListUnivs(vars, newactuals)) { 
	// already made, reuse it
	IdentList *t = (*i).actuals->copy();
	t->compress();
	newactuals->compress();
        res = VarCode(subst((*i).vc.vars, t, newactuals), (*i).vc.code);
	delete newactuals;
	delete t;
	break;
      }
    if (!res.code) {
      // not already made, make it and add to lists
      res = code->substCopy(newactuals);
      SubstCopy sc(newactuals, VarCode(res.vars->copy(), res.code));
      sc.sign = new Signature(*sc.actuals);
      code->sclist.push_front(sc);
      codeTable->addSC(sc);
    }
  }
  res.code->refs++;
  invariant(res.vars->size() == res.code->vars.size());
  return res;
}

VarCode
Code_True::substCopy(IdentList *)
{
  return codeTable->insert(new Code_True(pos));
}

VarCode
Code_False::substCopy(IdentList *)
{
  return codeTable->insert(new Code_False(pos));
}

VarCode
Code_EqEmpty::substCopy(IdentList *actuals)
{
  return codeTable->insert(new Code_EqEmpty(subst(id, &vars, actuals), pos));
}

VarCode
Code_EqRoot::substCopy(IdentList *actuals)
{
  return codeTable->insert(new Code_EqRoot(subst(id, &vars, actuals), 
					   copy(universes), pos));
}

VarCode
Code_FirstOrder::substCopy(IdentList *actuals)
{
  return codeTable->insert(new Code_FirstOrder(subst(id, &vars, actuals), pos));
}

VarCode
Code_EqConst::substCopy(IdentList *actuals)
{
  return codeTable->insert(new Code_EqConst(subst(id, &vars, actuals), val, pos));
}

VarCode
Code_Singleton::substCopy(IdentList *actuals)
{
  return codeTable->insert(new Code_Singleton(subst(id, &vars, actuals), pos));
}

VarCode
Code_BoolVar::substCopy(IdentList *actuals)
{
  return codeTable->insert(new Code_BoolVar(subst(id, &vars, actuals), pos));
}

VarCode
Code_InStateSpace::substCopy(IdentList *actuals)
{
  return codeTable->insert(new Code_InStateSpace(subst(id, &vars, actuals), 
						 copy(ss), pos));
}

VarCode
Code_SomeType::substCopy(IdentList *actuals)
{
  return codeTable->insert(new Code_SomeType(subst(id, &vars, actuals), pos));
}

VarCode
Code_In::substCopy(IdentList *actuals)
{
  return codeTable->insert(new Code_In(subst(id1, &vars, actuals), 
				       subst(id2, &vars, actuals), pos));
}

VarCode
Code_Eq1::substCopy(IdentList *actuals)
{
  return codeTable->insert(new Code_Eq1(subst(id1, &vars, actuals),
					subst(id2, &vars, actuals), pos));
}

VarCode
Code_Eq2::substCopy(IdentList *actuals)
{
  return codeTable->insert(new Code_Eq2(subst(id1, &vars, actuals),
					subst(id2, &vars, actuals), pos));
}

VarCode
Code_Sub2::substCopy(IdentList *actuals)
{
  return codeTable->insert(new Code_Sub2(subst(id1, &vars, actuals),
					 subst(id2, &vars, actuals), pos));
}

VarCode
Code_Less1::substCopy(IdentList *actuals)
{
  return codeTable->insert(new Code_Less1(subst(id1, &vars, actuals),
					  subst(id2, &vars, actuals), pos));
}

VarCode
Code_LessEq1::substCopy(IdentList *actuals)
{
  return codeTable->insert(new Code_LessEq1(subst(id1, &vars, actuals),
					    subst(id2, &vars, actuals), pos));
}

VarCode
Code_EqDot0::substCopy(IdentList *actuals)
{
  return codeTable->insert(new Code_EqDot0(subst(id1, &vars, actuals),
					   subst(id2, &vars, actuals), pos));
}

VarCode
Code_EqDot1::substCopy(IdentList *actuals)
{
  return codeTable->insert(new Code_EqDot1(subst(id1, &vars, actuals),
					   subst(id2, &vars, actuals), pos));
}

VarCode
Code_EqUp::substCopy(IdentList *actuals)
{
  return codeTable->insert(new Code_EqUp(subst(id1, &vars, actuals),
					 subst(id2, &vars, actuals), pos));
}

VarCode
Code_EqPlus2::substCopy(IdentList *actuals)
{
  return codeTable->insert(new Code_EqPlus2(subst(id1, &vars, actuals),
					    subst(id2, &vars, actuals), pos));
}

VarCode
Code_EqMinus2::substCopy(IdentList *actuals)
{
  return codeTable->insert(new Code_EqMinus2(subst(id1, &vars, actuals),
					     subst(id2, &vars, actuals), pos));
}

VarCode
Code_EqMin::substCopy(IdentList *actuals)
{
  return codeTable->insert(new Code_EqMin(subst(id1, &vars, actuals),
					  subst(id2, &vars, actuals), pos));
}

VarCode
Code_EqMax::substCopy(IdentList *actuals)
{
  return codeTable->insert(new Code_EqMax(subst(id1, &vars, actuals),
					  subst(id2, &vars, actuals), pos));
}

VarCode
Code_EqPlus1::substCopy(IdentList *actuals)
{
  return codeTable->insert(new Code_EqPlus1(subst(id1, &vars, actuals),
					    subst(id2, &vars, actuals), 
					    val, pos));
}

VarCode
Code_EqMinus1::substCopy(IdentList *actuals)
{
  return codeTable->insert(new Code_EqMinus1(subst(id1, &vars, actuals),
					     subst(id2, &vars, actuals), pos));
}

VarCode
Code_EqUnion::substCopy(IdentList *actuals)
{
  return codeTable->insert(new Code_EqUnion(subst(id1, &vars, actuals),
					    subst(id2, &vars, actuals), 
					    subst(id3, &vars, actuals), pos));
}

VarCode
Code_EqInter::substCopy(IdentList *actuals)
{
  return codeTable->insert(new Code_EqInter(subst(id1, &vars, actuals),
					    subst(id2, &vars, actuals), 
					    subst(id3, &vars, actuals), pos));
}

VarCode
Code_EqSetMinus::substCopy(IdentList *actuals)
{
  return codeTable->insert(new Code_EqSetMinus(subst(id1, &vars, actuals),
					       subst(id2, &vars, actuals), 
					       subst(id3, &vars, actuals), 
					       pos));
}

VarCode
Code_EqPlusModulo::substCopy(IdentList *actuals)
{
  return codeTable->insert(new Code_EqPlusModulo(subst(id1, &vars, actuals),
						 subst(id2, &vars, actuals), 
						 subst(id3, &vars, actuals), 
						 pos));
}

VarCode
Code_EqMinusModulo::substCopy(IdentList *actuals)
{
  return codeTable->insert(new Code_EqMinusModulo(subst(id1, &vars, actuals),
						  subst(id2, &vars, actuals), 
						  subst(id3, &vars, actuals), 
						  pos));
}

VarCode
Code_EqPresbConst::substCopy(IdentList *actuals)
{
  return codeTable->insert(new Code_EqPresbConst(subst(id, &vars, actuals), 
						 val, pos));
}

VarCode
Code_WellFormedTree::substCopy(IdentList *actuals)
{
  return codeTable->insert(new Code_WellFormedTree(subst(id, &vars, actuals), 
						   pos));
}

VarCode
Code_Restrict::substCopy(IdentList *actuals)
{
  return codeTable->insert(new Code_Restrict(vc.substCopy(&vars, actuals), 
					     pos));
}

VarCode
Code_Project::substCopy(IdentList *actuals)
{
  return codeTable->insert
    (new Code_Project(var, vc.substCopy(&vars, actuals), pos));
}

VarCode
Code_Negate::substCopy(IdentList *actuals)
{
  return codeTable->insert
    (new Code_Negate(vc.substCopy(&vars, actuals), pos));
}

VarCode
Code_Prefix::substCopy(IdentList *actuals)
{
  return codeTable->insert
    (new Code_Prefix(vc.substCopy(&vars, actuals), pos));
}

VarCode
Code_And::substCopy(IdentList *actuals)
{
  return codeTable->insert
    (new Code_And(vc1.substCopy(&vars, actuals), 
		  vc2.substCopy(&vars, actuals), pos));
}

VarCode
Code_IdLeft::substCopy(IdentList *actuals)
{
  return codeTable->insert
    (new Code_IdLeft(vc1.substCopy(&vars, actuals), 
		     vc2.substCopy(&vars, actuals), pos));
}

VarCode
Code_Or::substCopy(IdentList *actuals)
{
  return codeTable->insert
    (new Code_Or(vc1.substCopy(&vars, actuals), 
		 vc2.substCopy(&vars, actuals), pos));
}

VarCode
Code_Impl::substCopy(IdentList *actuals)
{
  return codeTable->insert
    (new Code_Impl(vc1.substCopy(&vars, actuals), 
		   vc2.substCopy(&vars, actuals), pos));
}

VarCode
Code_Biimpl::substCopy(IdentList *actuals)
{
  return codeTable->insert
    (new Code_Biimpl(vc1.substCopy(&vars, actuals), 
		     vc2.substCopy(&vars, actuals), pos));
}

VarCode
Code_PredCall::substCopy(IdentList *actuals)
{
  IdentList a, f;
  IdentList::iterator i;
  for (i = this->actuals.begin(); i != this->actuals.end(); i++)
    a.push_back(subst(*i, &vars, actuals));
  for (i = frees.begin(); i != frees.end(); i++)
    f.push_back(subst(*i, &vars, actuals));

  return codeTable->insert
    (new Code_PredCall
     (name,
      vc.substCopy(&vars, actuals),
      a,
      &f,
      sourcefile, 
      pos));
}

VarCode
Code_Import::substCopy(IdentList *actuals)
{
  IdentList *newactuals = new IdentList();
  IdentList::iterator i;
  for (i = this->actuals->begin(); i != this->actuals->end(); i++)
    newactuals->push_back(subst(*i, &vars, actuals));
  return codeTable->insert
    (new Code_Import(file, formals->copy(), newactuals, pos));
}

VarCode
Code_Export::substCopy(IdentList *actuals)
{
  return codeTable->insert
    (new Code_Export(vc.substCopy(&vars, actuals), file, &freevars, pos));
}
