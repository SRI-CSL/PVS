//
// code.cpp
//
// MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
// 
// Reproduction of all or part of this software is permitted for
// educational or research use on condition that this copyright notice is
// included in any copy. This software comes with no warranty of any
// kind. In no event will BRICS be liable for any damages resulting from
// use of this software.

#include <iostream.h>
#include <string.h>
#include "code.h"
#include "offsets.h"
#include "predlib.h"
#include "symboltable.h"
#include "env.h"
#include "lib.h"

extern Environment environment;
extern Offsets offsets;
extern PredicateLib predicateLib;
extern SymbolTable symbolTable;
extern CodeTable codeTable;
extern AutLib lib;

Ident lastPosVar;

////////// Progress ///////////////////////////////////////////////////////////

int makes = 0; // number of automata constructed
int prev = 0;

void 
PrintProgress()
{
  makes++;
  if (environment.printProgress) {
    int part = 100 * makes / codeTable.nodes;
    if (part == 100 || !environment.demo)
      if (part > prev) {
	if (environment.statistics || environment.intermediate) 
	  cout << part << "% completed\n";
	else {
	  cout << "\r" << part << "% completed";
	  if (part == 100)
	    cout << "\n";
	}
	flush(cout);
      }
    prev = part;
  }
}

////////// Substitution ///////////////////////////////////////////////////////

Ident
substitute(Subst *subst, Ident var) // substitute var according to subst
{
  if (subst) {
    for (unsigned i = 0; subst[i].formal != -1; i++) // terminated by .formal=-1
      if (subst[i].formal == var)
	return subst[i].actual;
  }
  return var;
}

////////// stateSpaces ////////////////////////////////////////////////////////

SSSet
stateSpaces(IdentList *univs) // find state spaces for univs
{
  SSSet set = (SSSet) mem_alloc(guide.numSs);

  for (unsigned i = 0; i < guide.numSs; i++)
    set[i] = 0; // clear all

  if (univs)
    for (Ident *id = univs->begin(); id != univs->end(); id++) {
      int u = symbolTable.lookupUnivNumber(*id);
      for (unsigned i = 0; i < guide.numUnivSS[u]; i++)
	set[guide.univSS[u][i]] = 1;
    }
 
  return set;
}

SSSet
stateSpaces(Ident id) // find state spaces for id
{
  return stateSpaces(symbolTable.lookupUnivs(id));
}

void
dumpStateSpaces(SSSet set)
{
  unsigned i;
    
  cout << " [";

  for (i = 0; i < guide.numSs; i++)
    if (set[i]) {
      cout << i;
      break;
    }
  for (i++; i < guide.numSs; i++)
    if (set[i])
      cout << "," << i;

  cout << "]";
  free(set);
}

void
dumpStateSpaces(Ident id)
{
  if (environment.mode != TREE)
    return;

  dumpStateSpaces(stateSpaces(id));
}

////////// IdentList //////////////////////////////////////////////////////////

bool
sameUnivs(Ident id1, Ident id2) // test id1's universes same as id2's
{
  return equal(symbolTable.lookupUnivs(id1),
	       symbolTable.lookupUnivs(id2));
}

Ident
replace(Ident id, IdentList *free, IdentList *vars) // lookup substituted
{
  if (!free)
    return id;

  for (IdentList::iterator i = free->begin(), j = vars->begin();
       i != free->end(); i++, j++)
    if (id == *j)
      return *i;

  invariant(false);
  return 0;
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

DFA* 
Code::DFATranslate(IdentList *free)
{
  if (!dfa) {
    if (environment.intermediate)
      dump();
    makeDFA();
    PrintProgress();
  }

  DFA *a = dfa;
  if (refs > 1)
    a = st_dfa_copy(a);
  st_dfa_replace_indices(a, free, vars); 
  return a;
}

GTA* 
Code::GTATranslate(IdentList *free)
{
  if (!gta) {
    if (environment.intermediate)
      dump();
    makeGTA();
    PrintProgress();
  }

  GTA *g = gta;
  if (refs > 1)
    g = st_gta_copy(g);
  st_gta_replace_indices(g, free, vars); 
  return g;
}

void 
Code::viz()
{
  if (!mark) {
    char *names[] = {
      "True", "False", "In", "Eq1", "Eq2", "Sub2", "Less1", "LessEq1", 
      "EqUnion", "EqInter", "EqSetMinus", "EqEmpty", "EqRoot", 
      "FirstOrder", "EqConst", "Singleton", "BoolVar", "PredCall",
      "EqDot0", "EqDot1", "EqUp", "EqPlus1", "EqMinus1", "EqPlus2",
      "EqMinus2", "EqPlusModulo", "EqMinusModulo", "EqMin", "EqMax",
      "And", "Or", "Impl", "Biimpl", "Restrict", "Project", "Negate",
      "External", "Import", "Export", "Prefix", "InStateSpace", 
      "IdLeft", "EqPresbConst"
    };

    printf(" L%x [label=\"%s\"];\n", (unsigned) this, names[kind]);
    mark = true;
  }
}

void
Code::remove() // remove one reference to this node
{
  if (--refs == 0) {
    codeTable.remove(this);
    delete vars;
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

////////// VarCode ////////////////////////////////////////////////////////////

void
VarCode::detach() // detach VarCode reference, use bottom-up only
{
  if (code) {
    if (code->vars != vars)
      delete vars;
    if (--(code->refs) == 0) {
      delete code->vars;
      delete code;
    }
    code = NULL;
  }
}

////////// Code_n /////////////////////////////////////////////////////////////

Code_n::Code_n(CodeKind knd, Ident i, Pos p) :
  Code(knd, p), id(i)
{
  vars = new IdentList();
  vars->push_back(id);
  size = 1;
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
  vars = new IdentList();
  vars->push_back(id1);
  vars->push_back(id2);
  sign = new Signature(*vars);
  size = 2;
}

bool
Code_nn::equiv(Code &c)
{
  return kind == c.kind && 
    *sign == *((Code_nn&) c).sign &&
    sameUnivs(id1, ((Code_nn&) c).id1) &&
    sameUnivs(id2, ((Code_nn&) c).id2); 
}

unsigned
Code_nn::hash() 
{
  return kind + sign->hashvalue*3;
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
    *sign == *((Code_nni&) c).sign &&
    val == ((Code_nni&) c).val &&
    sameUnivs(id1, ((Code_nni&) c).id1) &&
    sameUnivs(id2, ((Code_nni&) c).id2); 
}

unsigned
Code_nni::hash() 
{
  return kind + sign->hashvalue*3 + val*128;
}

////////// Code_nnn ///////////////////////////////////////////////////////////

Code_nnn::Code_nnn(CodeKind knd, Ident i, Ident j, Ident k, Pos p) :
  Code(knd, p), id1(i), id2(j), id3(k)
{
  vars = new IdentList();
  vars->push_back(id1);
  vars->push_back(id2);
  vars->push_back(id3);
  sign = new Signature(*vars);
  size = 3;
}

bool
Code_nnn::equiv(Code &c)
{
  return kind == c.kind &&
    *sign == *((Code_nnn&) c).sign &&
    sameUnivs(id1, ((Code_nnn&) c).id1) &&
    sameUnivs(id2, ((Code_nnn&) c).id2) &&
    sameUnivs(id3, ((Code_nnn&) c).id3); 
}

unsigned 
Code_nnn::hash() 
{
  return kind + sign->hashvalue*3;
}

////////// Code_c /////////////////////////////////////////////////////////////

Code_c::Code_c(CodeKind knd, VarCode c, Pos p, bool skipinit) :
  Code(knd, p), vc(c)
{
  if (!skipinit) 
    if (vc.vars) {
      vars = new IdentList();
      vars->append(vc.vars);
      sign = new Signature(*vars);
      size = vc.code->size;
    }
    else
      small = false;
}

bool
Code_c::equiv(Code &c)
{
  return kind == c.kind && 
    vc.code == ((Code_c&) c).vc.code &&
    small == c.small &&
    (small ? 
     (*sign == *((Code_c&) c).sign) : 
     (equal(vc.vars, ((Code_c&) c).vc.vars)));
}

unsigned 
Code_c::hash() 
{
  return kind + ((int) vc.code)*128;
}

void
Code_c::remove() // remove one reference to this node
{
  if (--refs == 0) {
    codeTable.remove(this);
    delete vars;
    if (vc.code) {
      if (vc.code->vars != vc.vars) {
	delete vc.vars;
	vc.vars = NULL;
      }
      vc.code->remove();
      vc.code = NULL;
    }
    delete this;
  }
}

void
Code_c::viz()
{
  if (!mark) {
    vc.code->viz();
    Code::viz();
    printf(" L%x -> L%x;\n", (unsigned) this, (unsigned) vc.code);
    mark = true;
  }
}

////////// Code_cc ////////////////////////////////////////////////////////////

Code_cc::Code_cc(CodeKind knd, VarCode c1, VarCode c2, Pos p):
  Code(knd, p), vc1(c1), vc2(c2) 
{
  small = false;

  if (vc1.code->small && vc2.code->small) {
    unsigned s = 0;
    if (vc1.vars)
      s += vc1.vars->size();
    if (vc2.vars)
      s += vc2.vars->size();
    
    if (s <= environment.reuseDegree) {
      vars = new IdentList();
      if (vc1.vars)
	vars->append(vc1.vars);
      if (vc2.vars)
	vars->append(vc2.vars);
      sign = new Signature(*vars);
      small = true;
    }
  }

  size = vc1.code->size + vc2.code->size;
}

bool
Code_cc::equiv(Code &c)
{
  return kind == c.kind && 
    vc1.code == ((Code_cc&) c).vc1.code && 
    vc2.code == ((Code_cc&) c).vc2.code &&
    small == c.small &&
    (small ? 
     (*sign == *((Code_cc&) c).sign) : 
     (equal(vc1.vars, ((Code_cc&) c).vc1.vars) &&
      equal(vc2.vars, ((Code_cc&) c).vc2.vars)));
}

unsigned 
Code_cc::hash() 
{
  return kind*2 + ((int) vc1.code)*5 + ((int) vc2.code)*7;
}

void
Code_cc::split(IdentList *free, Subst *subst) // split free for dumping
{
  if (free) {
    IdentList l1, l2;
    
    IdentList::iterator i;
    unsigned s;
    for (i = free->begin(), s = 0;
	 vc1.vars && s < vc1.vars->size(); i++, s++)
      l1.push_back(*i);
    for (s = 0;
	 vc2.vars && s < vc2.vars->size(); i++, s++)
      l2.push_back(*i);
    
    vc1.code->dump(&l1, subst);
    cout << ",";
    vc2.code->dump(&l2, subst);
  }
  else {
    vc1.code->dump(vc1.vars, subst);
    cout << ",";
    vc2.code->dump(vc2.vars, subst);
  }
  cout << ")";
}

void
Code_cc::remove() // remove one reference to this node
{
  if (--refs == 0) {
    codeTable.remove(this);
    delete vars;
    if (vc1.code) {
      if (vc1.code->vars != vc1.vars) {
	delete vc1.vars;
	vc1.vars = NULL;
      }
      vc1.code->remove();
      vc1.code = NULL;
    }
    if (vc2.code) {
      if (vc2.code->vars != vc2.vars) {
	delete vc2.vars;
	vc2.vars = NULL;
      }
      vc2.code->remove();
      vc2.code = NULL;
    }
    delete this;
  }
}

void
Code_cc::viz()
{
  if (!mark) {
    vc1.code->viz();
    vc2.code->viz();
    Code::viz();
    printf(" L%x -> L%x;\n"
	   " L%x -> L%x;\n", 
	   (unsigned) this, (unsigned) vc1.code, 
	   (unsigned) this, (unsigned) vc2.code);
    mark = true;
  }
}

////////// Code_True //////////////////////////////////////////////////////////

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
Code_True::dump(IdentList *, Subst *)
{
  cout << "True()";
}

void
Code_True::dump()
{
  cout << "True()\n";
}

////////// Code_False /////////////////////////////////////////////////////////

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
Code_False::dump(IdentList *, Subst *)
{
  cout << "False()";
}

void
Code_False::dump()
{
  cout << "False()\n";
}

////////// Code_EqEmpty ///////////////////////////////////////////////////////

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

void
Code_EqEmpty::dump(IdentList *free, Subst *subst)
{
  Ident i = replace(id, free, vars);
  
  cout << "Empty(#" << substitute(subst, offsets.off(i));
  dumpStateSpaces(i);
  cout << ")";
}

void
Code_EqEmpty::dump()
{
  cout << "Empty(#" << offsets.off(id) << ")\n";
}

////////// Code_EqRoot ////////////////////////////////////////////////////////

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
  gta = gtaRoot(offsets.off(id), stateSpaces(id), stateSpaces(universes));
}

void
Code_EqRoot::dump(IdentList *free, Subst *subst)
{
  Ident i = replace(id, free, vars);
  
  cout << "Root(#" << offsets.off(substitute(subst, i));
  dumpStateSpaces(i);
  cout << ",";
  dumpStateSpaces(stateSpaces(universes));
  cout << ")";
}

void
Code_EqRoot::dump()
{
  cout << "Root(#" << offsets.off(id) << ",";
  dumpStateSpaces(stateSpaces(universes));
  cout << ")";
}

////////// Code_FirstOrder ////////////////////////////////////////////////////

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
Code_FirstOrder::dump(IdentList *free, Subst *subst)
{
  Ident i = replace(id, free, vars);
  
  cout << "FirstOrder(#" << offsets.off(substitute(subst, i));
  dumpStateSpaces(i);
  cout << ")";
}

void
Code_FirstOrder::dump()
{
  cout << "FirstOrder(#" << offsets.off(id) << ")\n";
}

////////// Code_EqConst ///////////////////////////////////////////////////////

void 
Code_EqConst::makeDFA()
{
  dfa = dfaConst(val, offsets.off(id));
}

void
Code_EqConst::dump(IdentList *free, Subst *subst)
{
  Ident i = replace(id, free, vars);
  
  cout << "Const(#" << offsets.off(substitute(subst, i)) << "," << val << ")";
}

void
Code_EqConst::dump()
{
  cout << "Const(#" << offsets.off(id) << "," << val << ")\n";
}

////////// Code_Singleton /////////////////////////////////////////////////////

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
Code_Singleton::dump(IdentList *free, Subst *subst)
{
  Ident i = replace(id, free, vars);
  
  cout << "Singleton(#" << offsets.off(substitute(subst, i)) << ")";
}

void
Code_Singleton::dump()
{
  cout << "Singleton(#" << offsets.off(id) << ")\n";
}

////////// Code_BoolVar ///////////////////////////////////////////////////////

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

void
Code_BoolVar::dump(IdentList *free, Subst *subst)
{
  Ident i = replace(id, free, vars);
  
  cout << "BoolVar(#" << offsets.off(substitute(subst, i)) << ")";
}

void
Code_BoolVar::dump()
{
  cout << "BoolVar(#" << offsets.off(id) << ")\n";
}

////////// Code_InStateSpace //////////////////////////////////////////////////

bool 
Code_InStateSpace::equiv(Code &c)
{
  return kind == c.kind && 
    equal(ss, ((Code_InStateSpace&) c).ss) &&
    sameUnivs(id, ((Code_InStateSpace&) c).id); // signatures trivially identical
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
Code_InStateSpace::dump(IdentList *free, Subst *subst)
{
  Ident i = replace(id, free, vars);

  cout << "InStateSpace(#" << offsets.off(substitute(subst, i));
  dumpStateSpaces(i);
  cout << ",";
  ss->dump();
  cout << ")";
}

void
Code_InStateSpace::dump()
{
  cout << "InStateSpace(#" << offsets.off(id) << ",";
  ss->dump();
  cout << ")";
}

////////// Code_In ////////////////////////////////////////////////////////////

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
Code_In::dump(IdentList *free, Subst *subst)
{
  Ident i1 = replace(id1, free, vars);
  Ident i2 = replace(id2, free, vars);

  cout << "In(#" << offsets.off(substitute(subst, i1));
  dumpStateSpaces(i1);
  cout << ",#" << offsets.off(substitute(subst, i2));
  dumpStateSpaces(i2);
  cout << ")";
}

void
Code_In::dump()
{
  cout << "In(#" << offsets.off(id1) << ",#" << offsets.off(id2) << ")\n";
}

////////// Code_Eq1 ///////////////////////////////////////////////////////////

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
Code_Eq1::dump(IdentList *free, Subst *subst)
{
  Ident i1 = replace(id1, free, vars);
  Ident i2 = replace(id2, free, vars);
  
  cout << "Eq1(#" << offsets.off(substitute(subst, i1));
  dumpStateSpaces(i1);
  cout << ",#" << offsets.off(substitute(subst, i2));
  dumpStateSpaces(i2);
  cout << ")";
}

void
Code_Eq1::dump()
{
  cout << "Eq1(#" << offsets.off(id1) << ",#" << offsets.off(id2) << ")\n";
}

////////// Code_Eq2 ///////////////////////////////////////////////////////////

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
Code_Eq2::dump(IdentList *free, Subst *subst)
{
  Ident i1 = replace(id1, free, vars);
  Ident i2 = replace(id2, free, vars);
  
  cout << "Eq2(#" << offsets.off(substitute(subst, i1));
  dumpStateSpaces(i1);
  cout << ",#" << offsets.off(substitute(subst, i2));
  dumpStateSpaces(i2);
  cout << ")";
}

void
Code_Eq2::dump()
{
  cout << "Eq2(#" << offsets.off(id1) << ",#" << offsets.off(id2) << ")\n";
}

////////// Code_Sub2 //////////////////////////////////////////////////////////

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
Code_Sub2::dump(IdentList *free, Subst *subst)
{
  Ident i1 = replace(id1, free, vars);
  Ident i2 = replace(id2, free, vars);
  
  cout << "Sub(#" << offsets.off(substitute(subst, i1));
  dumpStateSpaces(i1);
  cout << ",#" << offsets.off(substitute(subst, i2));
  dumpStateSpaces(i2);
  cout << ")";
}

void
Code_Sub2::dump()
{
  cout << "Sub2(#" << offsets.off(id1) << ",#" << offsets.off(id2) << ")\n";
}

////////// Code_Less1 /////////////////////////////////////////////////////////

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
Code_Less1::dump(IdentList *free, Subst *subst)
{
  Ident i1 = replace(id1, free, vars);
  Ident i2 = replace(id2, free, vars);
  
  cout << "Less(#" << offsets.off(substitute(subst, i1));
  dumpStateSpaces(i1);
  cout << ",#" << offsets.off(substitute(subst, i2));
  dumpStateSpaces(i2);
  cout << ")";
}

void
Code_Less1::dump()
{
  cout << "Less1(#" << offsets.off(id1) << ",#" << offsets.off(id2) << ")\n";
}

////////// Code_LessEq1 ///////////////////////////////////////////////////////

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
Code_LessEq1::dump(IdentList *free, Subst *subst)
{
  Ident i1 = replace(id1, free, vars);
  Ident i2 = replace(id2, free, vars);
  
  cout << "LessEq(#" << offsets.off(substitute(subst, i1));
  dumpStateSpaces(i1);
  cout << ",#" << offsets.off(substitute(subst, i2));
  dumpStateSpaces(i2);
  cout << ")";
}

void
Code_LessEq1::dump()
{
  cout << "LessEq1(#" << offsets.off(id1) << ",#" << offsets.off(id2) << ")\n";
}

////////// Code_EqDot0 ////////////////////////////////////////////////////////

void 
Code_EqDot0::makeGTA()
{
  gta = gtaDot0(offsets.off(id1), offsets.off(id2), stateSpaces(id1));
}

void
Code_EqDot0::dump(IdentList *free, Subst *subst)
{
  Ident i1 = replace(id1, free, vars);
  Ident i2 = replace(id2, free, vars);
  
  cout << "Dot0(#" << offsets.off(substitute(subst, i1));
  dumpStateSpaces(i1);
  cout << ",#" << offsets.off(substitute(subst, i2)) << ")";
}

void
Code_EqDot0::dump()
{
  cout << "Dot0(#" << offsets.off(id1) << ",#" << offsets.off(id2) << ")";
}

////////// Code_EqDot1 ////////////////////////////////////////////////////////

void 
Code_EqDot1::makeGTA()
{
  gta = gtaDot1(offsets.off(id1), offsets.off(id2), stateSpaces(id1));
}

void
Code_EqDot1::dump(IdentList *free, Subst *subst)
{
  Ident i1 = replace(id1, free, vars);
  Ident i2 = replace(id2, free, vars);
  
  cout << "Dot1(#" << offsets.off(substitute(subst, i1));
  dumpStateSpaces(i1);
  cout << ",#" << offsets.off(substitute(subst, i2)) << ")";
}

void
Code_EqDot1::dump()
{
  cout << "Dot1(#" << offsets.off(id1) << ",#" << offsets.off(id2) << ")";
}

////////// Code_EqUp //////////////////////////////////////////////////////////

void 
Code_EqUp::makeGTA()
{
  gta = gtaUp(offsets.off(id1), offsets.off(id2), stateSpaces(id1));
}

void
Code_EqUp::dump(IdentList *free, Subst *subst)
{
  Ident i1 = replace(id1, free, vars);
  Ident i2 = replace(id2, free, vars);
  
  cout << "Up(#" << offsets.off(substitute(subst, i1));
  dumpStateSpaces(i1);
  cout << ",#" << offsets.off(substitute(subst, i2)) << ")";
}

void
Code_EqUp::dump()
{
  cout << "eqUp(#" << offsets.off(id1) << ",#" << offsets.off(id2) << ")\n";
}

////////// Code_EqPlus2 ///////////////////////////////////////////////////////

void 
Code_EqPlus2::makeDFA()
{
  dfa = dfaPlus2(offsets.off(id1), offsets.off(id2));
}

void
Code_EqPlus2::dump(IdentList *free, Subst *subst)
{
  Ident i1 = replace(id1, free, vars);
  Ident i2 = replace(id2, free, vars);
  
  cout << "Plus2(#" << offsets.off(substitute(subst, i1))
       << ",#" << offsets.off(substitute(subst, i2))
       << ")";
}

void
Code_EqPlus2::dump()
{
  cout << "EqPlus2(#" << offsets.off(id1) << ",#" 
       << offsets.off(id2) << ")\n";
}

////////// Code_EqMinus2 //////////////////////////////////////////////////////

void 
Code_EqMinus2::makeDFA()
{
  dfa = dfaMinus2(offsets.off(id1), offsets.off(id2));
}

void
Code_EqMinus2::dump(IdentList *free, Subst *subst)
{
  Ident i1 = replace(id1, free, vars);
  Ident i2 = replace(id2, free, vars);
  
  cout << "Minus2(#" << offsets.off(substitute(subst, i1))
       << ",#" << offsets.off(substitute(subst, i2))
       << ")";
}

void
Code_EqMinus2::dump()
{
  cout << "EqMinus2(#" << offsets.off(id1) << ",#" 
       << offsets.off(id2) << ")\n";
}

////////// Code_Min ///////////////////////////////////////////////////////////

void 
Code_EqMin::makeDFA()
{
  dfa = dfaMin(offsets.off(id1), offsets.off(id2));
}

void
Code_EqMin::dump(IdentList *free, Subst *subst)
{
  Ident i1 = replace(id1, free, vars);
  Ident i2 = replace(id2, free, vars);
  
  cout << "Min(#" << offsets.off(substitute(subst, i1))
       << ",#" << offsets.off(substitute(subst, i2))
       << ")";
}

void
Code_EqMin::dump()
{
  cout << "EqMin(#" << offsets.off(id1) << ",#" << offsets.off(id2) << ")\n";
}

////////// Code_Max ///////////////////////////////////////////////////////////

void 
Code_EqMax::makeDFA()
{
  dfa = dfaMax(offsets.off(id1), offsets.off(id2));
}

void
Code_EqMax::dump(IdentList *free, Subst *subst)
{
  Ident i1 = replace(id1, free, vars);
  Ident i2 = replace(id2, free, vars);
  
  cout << "Max(#" << offsets.off(substitute(subst, i1))
       << ",#" << offsets.off(substitute(subst, i2))
       << ")";
}

void
Code_EqMax::dump()
{
  cout << "EqMax(#" << offsets.off(id1) << ",#" << offsets.off(id2) << ")\n";
}

////////// Code_EqPlus1 ///////////////////////////////////////////////////////

void 
Code_EqPlus1::makeDFA()
{
  dfa = dfaPlus1(offsets.off(id1), offsets.off(id2), val);
}

void
Code_EqPlus1::dump(IdentList *free, Subst *subst)
{
  Ident i1 = replace(id1, free, vars);
  Ident i2 = replace(id2, free, vars);
  
  cout << "Plus1(#" << offsets.off(substitute(subst, i1))
       << ",#" << offsets.off(substitute(subst, i2))
       << "," << val
       << ")";
}

void
Code_EqPlus1::dump()
{
  cout << "EqPlus1(#" << offsets.off(id1) << ",#" 
       << offsets.off(id2) << "," << val << ")\n";
}

////////// Code_EqMinus1 //////////////////////////////////////////////////////

void 
Code_EqMinus1::makeDFA()
{
  dfa = dfaMinus1(offsets.off(id1), offsets.off(id2), val);
}

void
Code_EqMinus1::dump(IdentList *free, Subst *subst)
{
  Ident i1 = replace(id1, free, vars);
  Ident i2 = replace(id2, free, vars);
  
  cout << "Minus1(#" << offsets.off(substitute(subst, i1))
       << ",#" << offsets.off(substitute(subst, i2))
       << "," << val
       << ")";
}

void
Code_EqMinus1::dump()
{
  cout << "EqMinus1(#" << offsets.off(id1) << ",#" 
       << offsets.off(id2) << "," << val << ")\n";
}

////////// Code_EqUnion ///////////////////////////////////////////////////////

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
Code_EqUnion::dump(IdentList *free, Subst *subst)
{
  Ident i1 = replace(id1, free, vars);
  Ident i2 = replace(id2, free, vars);
  Ident i3 = replace(id3, free, vars);
  
  cout << "Union(#" << offsets.off(substitute(subst, i1));
  dumpStateSpaces(i1);
  cout << ",#" << offsets.off(substitute(subst, i2))
       << ",#" << offsets.off(substitute(subst, i3)) << ")";
}

void
Code_EqUnion::dump()
{
  cout << "Union(#" << offsets.off(id1) << ",#" << offsets.off(id2) 
       << ",#" << offsets.off(id3) << ")\n";
}

////////// Code_EqInter ///////////////////////////////////////////////////////

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
Code_EqInter::dump(IdentList *free, Subst *subst)
{
  Ident i1 = replace(id1, free, vars);
  Ident i2 = replace(id2, free, vars);
  Ident i3 = replace(id3, free, vars);
  
  cout << "Inter(#" << offsets.off(substitute(subst, i1));
  dumpStateSpaces(i1);
  cout << ",#" << offsets.off(substitute(subst, i2))
       << ",#" << offsets.off(substitute(subst, i3)) << ")";
}

void
Code_EqInter::dump()
{
  cout << "Inter(#" << offsets.off(id1) << ",#" << offsets.off(id2) 
       << ",#" << offsets.off(id3) << ")\n";
}

////////// Code_EqSetMinus ////////////////////////////////////////////////////

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
Code_EqSetMinus::dump(IdentList *free, Subst *subst)
{
  Ident i1 = replace(id1, free, vars);
  Ident i2 = replace(id2, free, vars);
  Ident i3 = replace(id3, free, vars);
  
  cout << "SetMinus(#" << offsets.off(substitute(subst, i1));
  dumpStateSpaces(i1);
  cout << ",#" << offsets.off(substitute(subst, i2))
       << ",#" << offsets.off(substitute(subst, i3)) << ")";
}

void
Code_EqSetMinus::dump()
{
  cout << "SetMinus(#" << offsets.off(id1) << ",#" << offsets.off(id2) 
       << ",#" << offsets.off(id3) << ")\n";
}

////////// Code_EqPlusModulo //////////////////////////////////////////////////

void
Code_EqPlusModulo::makeDFA()
{
  dfa = dfaPlusModulo1(offsets.off(id1), offsets.off(id2), offsets.off(id3));
}

void
Code_EqPlusModulo::dump(IdentList *free, Subst *subst)
{
  Ident i1 = replace(id1, free, vars);
  Ident i2 = replace(id2, free, vars);
  Ident i3 = replace(id3, free, vars);
  
  cout << "PlusModulo(#" << offsets.off(substitute(subst, i1))
       << ",#" << offsets.off(substitute(subst, i2))
       << ",#" << offsets.off(substitute(subst, i3))
       << ")";
}

void
Code_EqPlusModulo::dump()
{
  cout << "EqPlusModulo(#" << offsets.off(id1) << ",#" 
       << offsets.off(id2) << ",#" << offsets.off(id3) << ")\n";
}

////////// Code_EqMinusModulo /////////////////////////////////////////////////

void
Code_EqMinusModulo::makeDFA()
{
  dfa = dfaMinusModulo1(offsets.off(id1), offsets.off(id2), offsets.off(id3));
}

void
Code_EqMinusModulo::dump(IdentList *free, Subst *subst)
{
  Ident i1 = replace(id1, free, vars);
  Ident i2 = replace(id2, free, vars);
  Ident i3 = replace(id3, free, vars);
  
  cout << "MinusModulo(#" << offsets.off(substitute(subst, i1))
       << ",#" << offsets.off(substitute(subst, i2))
       << ",#" << offsets.off(substitute(subst, i3))
       << ")";
}

void
Code_EqMinusModulo::dump()
{
  cout << "EqMinusModulo(#" << offsets.off(id1) << ",#" 
       << offsets.off(id2) << ",#" << offsets.off(id3) << ")\n";
}

////////// Code_EqPresbConst /////////////////////////////////////////////////

void
Code_EqPresbConst::makeDFA()
{
  dfa = dfaPresbConst(offsets.off(id), val);
}

void
Code_EqPresbConst::dump(IdentList *free, Subst *subst)
{
  Ident i = replace(id, free, vars);
  
  cout << "PresbConst(#" << offsets.off(substitute(subst, i)) 
       << "," << val << ")";
}

void
Code_EqPresbConst::dump()
{
  cout << "PresbConst(#" << offsets.off(id) << "," << val << ")\n";
}

////////// Code_Restrict //////////////////////////////////////////////////////

void 
Code_Restrict::makeDFA()
{
  dfa = st_dfa_restrict(vc.code->DFATranslate(vc.vars), pos);
  vc.detach();
}

void 
Code_Restrict::makeGTA()
{
  gta = st_gta_restrict(vc.code->GTATranslate(vc.vars), pos);
  vc.detach();
}

void
Code_Restrict::dump(IdentList *free, Subst *subst)
{
  cout << "Restrict(";

  if (free) 
    vc.code->dump(free, subst);
  else 
    vc.code->dump(vc.vars, subst);

  cout << ")";
}

////////// Code_Project ///////////////////////////////////////////////////////

Code_Project::Code_Project(Ident n, VarCode c, Pos p) :
  Code_c(cProject, c, p, true), var(n)
{
  IdentList::iterator i;

  varpos = 0;
  if (vc.vars) {
    vars = new IdentList();
    for (i = vc.vars->begin(); i != vc.vars->end(); i++) {
      if (*i < var)
	varpos++; // find relative offset of var
      if (*i != var) {
	vars->push_back(*i);
	size++;
      }
    }
    sign = new Signature(*vars);
  }
  else 
    small = false;
}

bool
Code_Project::equiv(Code &c)
{
  return kind == c.kind && 
    varpos == ((Code_Project&) c).varpos && // same relative var offset pos.
    vc.code == ((Code_Project&) c).vc.code &&
    small == c.small &&
    (small ? 
     (*sign == *((Code_Project&) c).sign) : 
     (equal(vc.vars, ((Code_Project&) c).vc.vars))) &&
    sameUnivs(var, ((Code_Project&) c).var);
}

void 
Code_Project::makeDFA()
{
  dfa = st_dfa_minimization(st_dfa_project
			    (vc.code->DFATranslate(vc.vars), 
			     var, pos));
  vc.detach();
}

void 
Code_Project::makeGTA()
{
  gta = st_gta_minimization(st_gta_project
			    (vc.code->GTATranslate(vc.vars),
			     var, pos));
  vc.detach();
}

void
Code_Project::dump(IdentList *free, Subst *subst)
{
  cout << "Project(#" << var << ",";

  if (free) {
    IdentList f;
    IdentList::iterator i, j;

    for (i = vc.vars->begin(), j = free->begin();
	 i != vc.vars->end(); i++)
      if (*i == var)
	f.push_back(var);
      else
	f.push_back(*(j++));

    vc.code->dump(&f, subst);
  }
  else
    vc.code->dump(vc.vars, subst);

  cout << ")";
}

////////// Code_Negate ////////////////////////////////////////////////////////

void 
Code_Negate::makeDFA()
{
  dfa = st_dfa_negation(vc.code->DFATranslate(vc.vars), pos);
  vc.detach();
}

void 
Code_Negate::makeGTA()
{
  gta = st_gta_negation(vc.code->GTATranslate(vc.vars), pos);
  vc.detach();
}

void
Code_Negate::dump(IdentList *free, Subst *subst)
{
  cout << "Negate(";

  if (free) 
    vc.code->dump(free, subst);
  else 
    vc.code->dump(vc.vars, subst);

  cout << ")";
}

////////// Code_Prefix ////////////////////////////////////////////////////////

void 
Code_Prefix::makeDFA()
{
  dfa = st_dfa_minimization(st_dfa_prefix(vc.code->DFATranslate(vc.vars), pos));
  vc.detach();
}

void
Code_Prefix::dump(IdentList *free, Subst *subst)
{
  cout << "Prefix(";

  if (free) 
    vc.code->dump(free, subst);
  else 
    vc.code->dump(vc.vars, subst);

  cout << ")";
}

////////// Code_And ///////////////////////////////////////////////////////////

void 
Code_And::makeDFA()
{
  DFA *a1 = vc1.code->DFATranslate(vc1.vars);
  vc1.detach();
  DFA *a2 = vc2.code->DFATranslate(vc2.vars);
  vc2.detach();
  dfa = st_dfa_minimization(st_dfa_product(a1, a2, dfaAND, pos));
}

void 
Code_And::makeGTA()
{
  GTA *g1 = vc1.code->GTATranslate(vc1.vars);
  vc1.detach();
  GTA *g2 = vc2.code->GTATranslate(vc2.vars);
  vc2.detach();
  gta = st_gta_minimization(st_gta_product(g1, g2, gtaAND, pos));
}

void
Code_And::dump(IdentList *free, Subst *subst)
{
  cout << "And(";
  split(free, subst);
}

////////// Code_IdLeft ////////////////////////////////////////////////////////

void 
Code_IdLeft::makeDFA()
{
  DFA *a1 = vc1.code->DFATranslate(vc1.vars);
  vc1.detach();
  DFA *a2 = vc2.code->DFATranslate(vc2.vars);
  vc2.detach();
  dfaFree(a2);
  dfa = a1;
}

void 
Code_IdLeft::makeGTA()
{
  GTA *g1 = vc1.code->GTATranslate(vc1.vars);
  vc1.detach();
  GTA *g2 = vc2.code->GTATranslate(vc2.vars);
  vc2.detach();
  gta = g1;
  gtaFree(g2);
}

void
Code_IdLeft::dump(IdentList *free, Subst *subst)
{
  cout << "IdLeft(";
  split(free, subst);
}

////////// Code_Or ////////////////////////////////////////////////////////////

void 
Code_Or::makeDFA()
{
  DFA *a1 = vc1.code->DFATranslate(vc1.vars);
  vc1.detach();
  DFA *a2 = vc2.code->DFATranslate(vc2.vars);
  vc2.detach();
  dfa = st_dfa_minimization(st_dfa_product(a1, a2, dfaOR, pos));
}

void 
Code_Or::makeGTA()
{
  GTA *g1 = vc1.code->GTATranslate(vc1.vars);
  vc1.detach();
  GTA *g2 = vc2.code->GTATranslate(vc2.vars);
  vc2.detach();
  gta = st_gta_minimization(st_gta_product(g1, g2, gtaOR, pos));
}

void
Code_Or::dump(IdentList *free, Subst *subst)
{
  cout << "Or(";
  split(free, subst);
}

////////// Code_Impl //////////////////////////////////////////////////////////

void 
Code_Impl::makeDFA()
{
  DFA *a1 = vc1.code->DFATranslate(vc1.vars);
  vc1.detach();
  DFA *a2 = vc2.code->DFATranslate(vc2.vars);
  vc2.detach();
  dfa = st_dfa_minimization(st_dfa_product(a1, a2, dfaIMPL, pos));
}

void 
Code_Impl::makeGTA()
{
  GTA *g1 = vc1.code->GTATranslate(vc1.vars);
  vc1.detach();
  GTA *g2 = vc2.code->GTATranslate(vc2.vars);
  vc2.detach();
  gta = st_gta_minimization(st_gta_product(g1, g2, gtaIMPL, pos));
}

void
Code_Impl::dump(IdentList *free, Subst *subst)
{
  cout << "Impl(";
  split(free, subst);
}

////////// Code_Biimpl ////////////////////////////////////////////////////////

void 
Code_Biimpl::makeDFA()
{
  DFA *a1 = vc1.code->DFATranslate(vc1.vars);
  vc1.detach();
  DFA *a2 = vc2.code->DFATranslate(vc2.vars);
  vc2.detach();
  dfa = st_dfa_minimization(st_dfa_product(a1, a2, dfaBIIMPL, pos));
}

void 
Code_Biimpl::makeGTA()
{
  GTA *g1 = vc1.code->GTATranslate(vc1.vars);
  vc1.detach();
  GTA *g2 = vc2.code->GTATranslate(vc2.vars);
  vc2.detach();
  gta = st_gta_minimization(st_gta_product(g1, g2, gtaBIIMPL, pos));
}

void
Code_Biimpl::dump(IdentList *free, Subst *subst)
{
  cout << "Biimpl(";
  split(free, subst);
}

////////// Code_PredCall //////////////////////////////////////////////////////

Code_PredCall::Code_PredCall(Ident n, VarCode c, IdentList &acts, 
			     IdentList *frees, char *fileName, Pos p) :
  Code_c(cPredCall, c, p, true), name(n), file(fileName) 
{
  // predcalls are always using signature equivalence
  // based on the actuals+frees,
  // all occuring variables must be in the substitution list
  vars = new IdentList(); 
  vars->append(&acts);
  vars->append(frees); // (might create doublets)
  sign = new Signature(*vars);
  size = vars->size(); // always SMALL

  // make mirror of signature
  s = new IdentList();
  unsigned i;
  for (i = 0; i < sign->size; i++)
    s->push_back(sign->sign[i]);
  
  // get names and orders
  names = new char*[vars->size()+1];
  orders = new int[vars->size()+1];
  for (i = 0; i < vars->size()+1; i++)
    names[i] = NULL;
  for (i = 0; i < vars->size(); i++) {
    names[s->get(i)] = symbolTable.lookupSymbol(vars->get(i));
    orders[s->get(i)] = symbolTable.lookupOrder(vars->get(i));
  }
}

bool
Code_PredCall::equiv(Code &c)
{
  if (!(kind == c.kind &&
	name == ((Code_PredCall&) c).name &&
	*sign == *((Code_PredCall&) c).sign &&
	vars->size() == ((Code_PredCall&) c).vars->size()))
    return false;
  for (IdentList::iterator i = vars->begin(),
	 j = ((Code_PredCall&) c).vars->begin();
       i != vars->end(); i++)
    if (!sameUnivs(*i, *j)) // check variable state spaces
      return false;
  return true;
}

unsigned
Code_PredCall::hash()
{
  return kind + sign->hashvalue*3 + name*128;
}

void 
Code_PredCall::makeDFA()
{
  if (environment.statistics)
    cout << "-- Entering predicate '" << symbolTable.lookupSymbol(name)
         << "' --\n";

  dfa = vc.code->DFATranslate(vc.vars);
  st_dfa_replace_indices(dfa, s, vars, false, true);
  if (environment.separateCompilation) {
    if (environment.statistics)
      cout << "-- Exporting '" << file << "' --\n";
    if (!dfaExport(dfa, file, names, orders)) 
      error("Unable to write file");
  }
  st_dfa_replace_indices(dfa, vars, s, true, false);
  vc.detach();

  if (environment.statistics)
    cout << "-- Leaving predicate '" << symbolTable.lookupSymbol(name)
         << "' (states: " << dfa->ns << ") --\n";
}

void 
Code_PredCall::makeGTA()
{
  if (environment.statistics)
    cout << "-- Entering predicate '" << symbolTable.lookupSymbol(name)
         << "' --\n";

  gta = vc.code->GTATranslate(vc.vars);
  st_gta_replace_indices(gta, s, vars, false, true);

  if (environment.separateCompilation) {
    SSSet *statespaces = new SSSet[vars->size()];
    for (unsigned i = 0; i < vars->size(); i++)
      statespaces[s->get(i)] = stateSpaces(vars->get(i));

    if (environment.statistics)
      cout << "-- Exporting '" << file << "' --\n";
    if (!gtaExport(gta, file, names, orders, statespaces)) 
      error("Unable to write file");

    for (unsigned i = 0; i < vars->size(); i++)
      if (statespaces[s->get(i)]) {
	free(statespaces[s->get(i)]);
	statespaces[s->get(i)] = 0;
      }
    delete[] statespaces;
  }

  st_gta_replace_indices(gta, vars, s, true, false);
  vc.detach();

  if (environment.statistics) {
    unsigned i, s = 0;
    for (i = 0; i < guide.numSs; i++)
      s += gta->ss[i].size;
    cout << "-- Leaving predicate '" << symbolTable.lookupSymbol(name)
	 << "' (states: " << s << ") --\n";
  }
}

void
Code_PredCall::dump(IdentList *free, Subst *subst)
{
  cout << "PredCall(" << symbolTable.lookupSymbol(name) << ",";

  Subst *newsubst = new Subst[vars->size() + 1];
  IdentList::iterator i, j;
  unsigned k;
  
  for (i = free->begin(), j = vars->begin(), k = 0;
       i != free->end(); i++, j++, k++) {
    newsubst[k].formal = *j;
    newsubst[k].actual = substitute(subst, *i);
  }
  newsubst[k].formal = -1; // end of array
  
  vc.code->dump(vc.vars, newsubst);
  
  delete[] newsubst;

  cout << ")";
}

////////// Code_External //////////////////////////////////////////////////////

Code_External::Code_External(char *fileName, 
			     IdentList &acts, IdentList *frees, Pos p) :
  Code(cExternal, p), file(fileName)
{
  vars = new IdentList(); 
  vars->append(&acts);
  vars->append(frees);
  sign = new Signature(*vars);
  size = vars->size(); // always SMALL

  // make mirror of signature
  s = new IdentList();
  unsigned i;
  for (i = 0; i < sign->size; i++)
    s->push_back(sign->sign[i]);
}

bool
Code_External::equiv(Code &c)
{
  return kind == c.kind &&
    strcmp(file, ((Code_External&) c).file) == 0;
}

unsigned
Code_External::hash()
{
  return kind + sign->hashvalue*3;
}

void
Code_External::makeDFA()
{
  if (environment.statistics)
    cout << "-- Importing '" << file << "' --\n";
  
  dfa = dfaImport(file, NULL, NULL);
  if (!dfa)
    error((String) "Error reading file '" + file + "'");

  st_dfa_replace_indices(dfa, vars, s, true, false); 
}

void
Code_External::makeGTA()
{
  if (environment.statistics)
    cout << "-- Importing '" << file << "' --\n";

  gta = gtaImport(file, NULL, NULL, NULL, false);
  if (!gta)
    error((String) "Error reading file '" + file + "'");

  st_gta_replace_indices(gta, vars, s, true, false); 
}

void
Code_External::dump(IdentList *, Subst *)
{
  cout << "External(" << file << ")"; // should dump free+subst
}

////////// Code_Import ////////////////////////////////////////////////////////

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
  if (environment.statistics)
    cout << "-- Importing '" << file << "' --\n";

  char **fileVars; // null terminated array of names in index order
  int *fileOrders; // corresponding array of orders

  dfa = dfaImport(file, &fileVars, &fileOrders);
  if (!dfa) 
    error((String) "Error reading file '" + file + "'");

  IdentList *off = getOffsets(fileVars, fileOrders, NULL);
  st_dfa_replace_indices(dfa, actuals, off, true, false); 

  for (int i = 0; fileVars[i]; i++)
    free(fileVars[i]);
  free(fileVars);
  free(fileOrders);
  delete off;
}

void
Code_Import::makeGTA()
{
  if (environment.statistics)
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
    free(fileVars[i]);
    free(fileSS[i]);
  }
  free(fileVars);
  free(fileOrders);
  free(fileSS);
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

void
Code_Import::dump(IdentList *, Subst *)
{
  cout << "Import(" << file << ")"; // should dump actuals/+formals
}

////////// Code_Export ////////////////////////////////////////////////////////

Code_Export::Code_Export(VarCode c, IdentList *freevars, 
			 char *fileName, Pos p) :
  Code_c(cExport, c, p), 
  frees(freevars), file(fileName)
{
  unsigned i;

  Signature t(*frees);
  s = new IdentList();
  for (i = 0; i < t.size; i++) 
    s->push_back(t.sign[i]);
  
  names = new char*[frees->size()+1];
  orders = new int[frees->size()];
  for (i = 0; i < frees->size()+1; i++)
    names[i] = NULL;
  for (i = 0; i < frees->size(); i++) {
    names[s->get(i)] = symbolTable.lookupSymbol(frees->get(i));
    orders[s->get(i)] = symbolTable.lookupOrder(frees->get(i));
  }
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
  dfa = vc.code->DFATranslate(vc.vars);
  
  if (environment.statistics)
    cout << "-- Exporting '" << file << "' --\n";
  
  DFA *dfa2 = dfaCopy(dfa);
  if (lastPosVar != -1)
    dfa2 = st_dfa_lastpos(dfa2, offsets.off(lastPosVar));
  if (environment.unrestrict) {
    dfaUnrestrict(dfa2);
    DFA *t = dfa2;
    dfa2 = dfaMinimize(dfa2);
    dfaFree(t);
  }
  st_dfa_replace_indices(dfa2, s, frees, false, true);
  if (!dfaExport(dfa2, file, names, orders))
    error("Unable to write file");
  dfaFree(dfa2);
  
  vc.detach();
}

void 
Code_Export::makeGTA()
{
  gta = vc.code->GTATranslate(vc.vars);
  
  if (environment.statistics)
    cout << "-- Exporting '" << file << "' --\n";
  
  SSSet *statespaces = new SSSet[frees->size()];
  for (unsigned i = 0; i < frees->size(); i++)
    statespaces[s->get(i)] = stateSpaces(frees->get(i));

  GTA *gta2 = gtaCopy(gta);
  if (lastPosVar != -1)
    gta2 = st_gta_lastpos(gta2, offsets.off(lastPosVar));
  if (environment.unrestrict) {
    gtaUnrestrict(gta2);
    GTA *t = gta2;
    gta2 = gtaMinimize(gta2);
    gtaFree(t);
  }
  st_gta_replace_indices(gta2, s, frees, false, true);
  if (!gtaExport(gta2, file, names, orders, statespaces)) 
    error("Unable to write file");
  gtaFree(gta2);

  for (unsigned i = 0; i < frees->size(); i++)
    free(statespaces[s->get(i)]);
  delete[] statespaces;
  
  vc.detach();
}

void
Code_Export::dump(IdentList *free, Subst *subst)
{
  cout << "Export(\"" << file << "\",";

  if (free) 
    vc.code->dump(free, subst);
  else 
    vc.code->dump(vc.vars, subst);

  cout << ")";
}

////////// CodeTable //////////////////////////////////////////////////////////

VarCode
CodeTable::insert(Code *c)
{
  return insert(VarCode(c->vars, c));
}

VarCode
CodeTable::insert(VarCode vc)
{
  unsigned hash = vc.code->hash() % CODE_TABLE_SIZE;
  
  if (environment.reuseDegree > 0 || vc.code->kind == cPredCall)
    for (Code **i = table[hash].begin(); i != table[hash].end(); i++) 
      if ((*i)->equiv(*vc.code)) {
	stat_hits++;
	(*i)->refs++;
	delete vc.code; // replace Code-node in VarCode with old node
	return VarCode(vc.vars, *i);
      }
  stat_misses++;
  nodes++;
  table[hash].push_back(vc.code);
  return vc;
}

bool
CodeTable::exists(Code &c)
{ 
  unsigned hash = c.hash() % CODE_TABLE_SIZE;
  
  if (environment.reuseDegree > 0 || c.kind == cPredCall)
    for (Code **i = table[hash].begin(); i != table[hash].end(); i++)
      if ((*i)->equiv(c))
	return true;
  return false;
}

void
CodeTable::remove(Code *c)
{
  unsigned hash = c->hash() % CODE_TABLE_SIZE;
  
  Deque<Code*>::iterator i;
  int j;
  for (i = table[hash].begin(), j = 0; i != table[hash].end(); i++, j++)
    if (*i == c) {
      if (i+1 == table[hash].end())
	table[hash].pop_back();
      else
	table[hash].set(j, table[hash].pop_back());
      nodes--;
      return;
    }
}

void 
CodeTable::reset()
{
  int i;
  for (i = 0; i < CODE_TABLE_SIZE; i++)
    table[i].reset();

  stat_hits = stat_misses = nodes = 0;
}

void
CodeTable::print_statistics()
{
  cout << "DAG hits: " << stat_hits
       << ", misses: " << stat_misses 
       << ", nodes: " << nodes << "\n";
}

void
CodeTable::print_sizes()
{
  unsigned i, j;
  cout << "\nDAG hash table bucket sizes:\n";
  for (i = 0; i < CODE_TABLE_SIZE; i++) 
    if (table[i].size() > 0) {
      cout << i << ": ";
      for (j = 0; j < table[i].size(); j++)
	cout << "*";
      cout << "\n";
    }
}
