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

#ifndef __CODE_H
#define __CODE_H

#include "deque.h"
#include "signature.h"
#include "st_dfa.h"
#include "st_gta.h"
#include "printline.h"
#include "ident.h"
#include "string.h"

////////// StateSpaces ////////////////////////////////////////////////////////

SSSet stateSpaces(IdentList *univs); // find state spaces for univs
SSSet stateSpaces(Ident id); // find state spaces for id
void dumpStateSpaces(Ident id);

////////// Code ///////////////////////////////////////////////////////////////

enum CodeKind {
  cTrue, cFalse, cIn, cEq1, cEq2, cSub2, cLess1, cLessEq1, cEqUnion,
  cEqInter, cEqSetMinus, cEqEmpty, cEqRoot, cFirstOrder, cEqConst,
  cSingleton, cBoolVar, cPredCall, cEqDot0, cEqDot1, cEqUp, cEqPlus1,
  cEqMinus1, cEqPlus2, cEqMinus2, cEqPlusModulo, cEqMinusModulo,
  cEqMin, cEqMax, cAnd, cOr, cImpl, cBiimpl, cRestrict, cProject,
  cNegate, cImport, cExport, cPrefix, cInStateSpace, cIdLeft, 
  cEqPresbConst, cWellFormedTree, cSomeType
};

class Code;

class VarCode {
public:
  VarCode(IdentList *v, Code *c) :
    vars(v), code(c) {}
  VarCode() : 
    vars(NULL), code(NULL) {}
  
  bool operator==(const VarCode &vc) {return vc.vars==vars && vc.code==code;}

  // generate DFA/GTA
  DFA *DFATranslate(); 
  GTA *GTATranslate(); 

  // reduction (reduce.cpp)
  void reduceAll(Deque<VarCode> *vcl);
  void reduce();
  void unmark();
  void setmark(int val);

  // copying (codesubst.cpp)
  VarCode substCopy(IdentList *formals, IdentList *actuals);

  // dump subtree contents
  void dump(bool rec = true);
  void dumpsubst();

  // remove reference
  void remove();

  IdentList *vars; // overriding free variables (can be same as code.vars)
  Code      *code; // code node
};

class VarCodeList: public Deque<VarCode> 
{
public:
  VarCodeList() : Deque<VarCode>() {}
  ~VarCodeList();

  void insert(IdentList *v, Code *c);
  void insert(VarCodeList *l);
  bool sub(VarCodeList *l1, VarCodeList *l2 = NULL);
  void dump();
};

class SubstCopy {
public:
  SubstCopy() : 
    actuals(NULL) {}
  SubstCopy(IdentList *acts, VarCode c) :
    actuals(acts), sign(NULL), vc(c) {}

  bool operator==(const SubstCopy &sc) 
  {return vc==sc.vc && equal(actuals, sc.actuals);}

  IdentList *actuals;
  Signature *sign;
  VarCode    vc;
};

class Code {
public:
  Code(CodeKind knd, Pos p) :
    kind(knd), refs(1), pos(p), mark(0), eqlist(NULL), dfa(NULL), gta(NULL),
    conj(NULL), restrconj(NULL) {}
  virtual ~Code() {}

  // determine syntax/signature equivalence
  virtual bool equiv(Code &c); 

  // hash node contents for CodeTable
  virtual unsigned hash(); 

  // make DFA/GTA for this node
  virtual void makeDFA() {}
  virtual void makeGTA() {}

  // remove one reference to this node, if last then call recursively
  void remove();

  // dump node/subtree contents
  virtual void viz(); // graphviz format
  virtual void dump(bool rec) = 0; // dump recursively/non-recursively
  virtual void show(); // print one line info for leaves

  // reduction (reduce.cpp)
  virtual void reduce1() {}
  virtual void reduce2();
  virtual void setmark(int val);
  virtual void clearEqlist();
  virtual bool checkExport(Ident x);

  // copying (codesubst.cpp)
  virtual VarCode substCopy(IdentList *actuals) = 0;

  // write error message
  void error(String);

  IdentList  vars;         // compressed default free variables
  Signature  sign;         // signature of free variables
  CodeKind   kind;         // object kind tag
  int        refs;         // number of DAG references to this node
  Pos        pos;          // source position
  int        mark;         // used for traversal
  VarCode    forwarded;    // used during reduction, none if .code=NULL
  unsigned   bucket;       // entry in codeTable, set when inserted
  Deque<SubstCopy> sclist; // used during substCopy (dyn. prog.)
  IdentList *eqlist;       // used during findEquality (dyn. prog.)
  DFA       *dfa;          // default DFA, NULL if not generated yet
  GTA       *gta;          // default GTA, NULL if not generated yet
  VarCodeList *conj;       // conjuncts (used during red.)
  VarCodeList *restrconj;  // restricted conjuncts (used during red.)
};

////////// Abstract classes ///////////////////////////////////////////////////

class Code_n: public Code {
public:
  Code_n(CodeKind knd, Ident i, Pos p);

  bool equiv(Code&);
  unsigned hash();

  Ident id;
};

class Code_ni: public Code_n {
public:
  Code_ni(CodeKind knd, Ident i, int n, Pos p);

  bool equiv(Code&);
  unsigned hash();

  int val;
};

class Code_nn: public Code {
public:
  Code_nn(CodeKind knd, Ident i, Ident j, Pos p);

  bool equiv(Code&);
  unsigned hash();

  Ident id1;
  Ident id2;
};

class Code_nni: public Code_nn {
public:
  Code_nni(CodeKind knd, Ident i, Ident j, int n, Pos p);

  bool equiv(Code&);
  unsigned hash();

  int val;
};

class Code_nnn: public Code {
public:
  Code_nnn(CodeKind knd, Ident i, Ident j, Ident k, Pos p);

  bool equiv(Code&);
  unsigned hash();

  Ident id1;
  Ident id2;
  Ident id3;
};

class Code_c: public Code {
public:
  Code_c(CodeKind knd, VarCode c, Pos p, bool skipinit=false);
  ~Code_c() {vc.remove();}

  bool equiv(Code&);
  unsigned hash();
  void setmark(int val);
  void viz();
  void reduce1();
  virtual void reduce2();
  void clearEqlist();
  virtual bool checkExport(Ident x);
  void show() {};

  VarCode vc;
};

class Code_cc: public Code {
public:
  Code_cc(CodeKind knd, VarCode c1, VarCode c2, Pos p);
  ~Code_cc() {vc1.remove(); vc2.remove();}

  bool equiv(Code&);
  unsigned hash();
  void setmark(int val);
  void viz();
  void reduce1();
  virtual void reduce2();
  void clearEqlist();
  bool checkExport(Ident x);
  void show() {};

  VarCode vc1;
  VarCode vc2;
};

////////// Atomic formulas ////////////////////////////////////////////////////

class Code_True: public Code {
public:
  Code_True(Pos p) :
    Code(cTrue, p) {}

  void makeDFA();
  void makeGTA();
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);
};

class Code_False: public Code {
public:
  Code_False(Pos p) :
    Code(cFalse, p) {}

  void makeDFA();
  void makeGTA();
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);
};

class Code_EqEmpty: public Code_n {
public:
  Code_EqEmpty(Ident i, Pos p) :
    Code_n(cEqEmpty, i, p) {}

  void makeDFA();
  void makeGTA();
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);
};

class Code_EqRoot: public Code_n {
public:
  Code_EqRoot(Ident i, IdentList *univs, Pos p) :
    Code_n(cEqRoot, i, p), universes(univs) {}
  ~Code_EqRoot() {delete universes;}

  bool equiv(Code&);
  void makeGTA();
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);

  IdentList *universes;
};

class Code_FirstOrder: public Code_n {
public:
  Code_FirstOrder(Ident i, Pos p) :
    Code_n(cFirstOrder, i, p) {}

  void makeDFA();
  void makeGTA();
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);
};

class Code_EqConst: public Code_ni {
public:
  Code_EqConst(Ident i, int n, Pos p) :
    Code_ni(cEqConst, i, n, p) {}

  void makeDFA();
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);
};

class Code_Singleton: public Code_n {
public:
  Code_Singleton(Ident i, Pos p) :
    Code_n(cSingleton, i, p) {}

  void makeDFA();
  void makeGTA();
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);
};

class Code_BoolVar: public Code_n {
public:
  Code_BoolVar(Ident i, Pos p) :
    Code_n(cBoolVar, i, p) {}

  void makeDFA();
  void makeGTA();
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);
};

class Code_InStateSpace: public Code_n {
public:
  Code_InStateSpace(Ident i, IdentList *s, Pos p) :
    Code_n(cInStateSpace, i, p), ss(s) {}
  ~Code_InStateSpace() {delete ss;}

  bool equiv(Code&);
  unsigned hash();
  void makeGTA();
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);

  IdentList *ss;
};

class Code_SomeType: public Code_n {
public:
  Code_SomeType(Ident i, Pos p) :
    Code_n(cSomeType, i, p) {}

  void makeGTA();
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);
};

class Code_In: public Code_nn {
public:
  Code_In(Ident i, Ident j, Pos p) :
    Code_nn(cIn, i, j, p) {}

  void makeDFA();
  void makeGTA();
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);
};


class Code_Eq1: public Code_nn {
public:
  Code_Eq1(Ident i, Ident j, Pos p) :
    Code_nn(cEq1, i, j, p) {}

  void makeDFA();
  void makeGTA();
  void dump(bool rec);
  void reduce1();
  VarCode substCopy(IdentList *actuals);
};

class Code_Eq2: public Code_nn {
public:
  Code_Eq2(Ident i, Ident j, Pos p) :
    Code_nn(cEq2, i, j, p) {}

  void makeDFA();
  void makeGTA();
  void dump(bool rec);
  void reduce1();
  VarCode substCopy(IdentList *actuals);
};

class Code_Sub2: public Code_nn {
public:
  Code_Sub2(Ident i, Ident j, Pos p) :
    Code_nn(cSub2, i, j, p) {}

  void makeDFA();
  void makeGTA();
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);
};

class Code_Less1: public Code_nn {
public:
  Code_Less1(Ident i, Ident j, Pos p) :
    Code_nn(cLess1, i, j, p) {}

  void makeDFA();
  void makeGTA();
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);
};

class Code_LessEq1: public Code_nn {
public:
  Code_LessEq1(Ident i, Ident j, Pos p) :
    Code_nn(cLessEq1, i, j, p) {}

  void makeDFA();
  void makeGTA();
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);
};

class Code_EqDot0: public Code_nn {
public:
  Code_EqDot0(Ident i, Ident j, Pos p) :
    Code_nn(cEqDot0, i, j, p) {}

  void makeGTA();
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);
};

class Code_EqDot1: public Code_nn {
public:
  Code_EqDot1(Ident i, Ident j, Pos p) :
    Code_nn(cEqDot1, i, j, p) {}

  void makeGTA();
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);
};

class Code_EqUp: public Code_nn {
public:
  Code_EqUp(Ident i, Ident j, Pos p) :
    Code_nn(cEqUp, i, j, p) {}

  void makeGTA();
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);
};

class Code_EqPlus2: public Code_nn {
public:
  Code_EqPlus2(Ident i, Ident j, Pos p) :
    Code_nn(cEqPlus2, i, j, p) {}

  void makeDFA();
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);
};

class Code_EqMinus2: public Code_nn {
public:
  Code_EqMinus2(Ident i, Ident j, Pos p) :
    Code_nn(cEqMinus2, i, j, p) {}

  void makeDFA();
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);
};

class Code_EqMin: public Code_nn {
public:
  Code_EqMin(Ident i, Ident j, Pos p) :
    Code_nn(cEqMin, i, j, p) {}

  void makeDFA();
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);
};

class Code_EqMax: public Code_nn {
public:
  Code_EqMax(Ident i, Ident j, Pos p) :
    Code_nn(cEqMax, i, j, p) {}

  void makeDFA();
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);
};

class Code_EqPlus1: public Code_nni {
public:
  Code_EqPlus1(Ident i, Ident j, int n, Pos p) :
    Code_nni(cEqPlus1, i, j, n, p) {}

  void makeDFA();
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);
};

class Code_EqMinus1: public Code_nn {
public:
  Code_EqMinus1(Ident i, Ident j, Pos p) :
    Code_nn(cEqMinus1, i, j, p) {}

  void makeDFA();
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);
};

class Code_EqUnion: public Code_nnn {
public:
  Code_EqUnion(Ident i, Ident j, Ident k, Pos p) :
    Code_nnn(cEqUnion, i, j, k, p) {}

  void makeDFA();
  void makeGTA();
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);
};

class Code_EqInter: public Code_nnn {
public:
  Code_EqInter(Ident i, Ident j, Ident k, Pos p) :
    Code_nnn(cEqInter, i, j, k, p) {}

  void makeDFA();
  void makeGTA();
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);
};

class Code_EqSetMinus: public Code_nnn {
public:
  Code_EqSetMinus(Ident i, Ident j, Ident k, Pos p) :
    Code_nnn(cEqSetMinus, i, j, k, p) {}

  void makeDFA();
  void makeGTA();
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);
};

class Code_EqPlusModulo: public Code_nnn {
public:
  Code_EqPlusModulo(Ident i, Ident j, Ident k, Pos p) :
    Code_nnn(cEqPlusModulo, i, j, k, p) {}

  void makeDFA();
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);
};

class Code_EqMinusModulo: public Code_nnn {
public:
  Code_EqMinusModulo(Ident i, Ident j, Ident k, Pos p) :
    Code_nnn(cEqMinusModulo, i, j, k, p) {}

  void makeDFA();
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);
};

class Code_EqPresbConst: public Code_ni {
public:
  Code_EqPresbConst(Ident i, int v, Pos p) :
    Code_ni(cEqPresbConst, i, v, p) {}

  void makeDFA();
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);
};

class Code_WellFormedTree: public Code_n {
public:
  Code_WellFormedTree(Ident i, Pos p) :
    Code_n(cWellFormedTree, i, p) {}

  void makeGTA();
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);
};

////////// Composite formulas /////////////////////////////////////////////////

class Code_Restrict: public Code_c {
public:
  Code_Restrict(VarCode vc, Pos p) :
    Code_c(cRestrict, vc, p) {}

  void makeDFA();
  void makeGTA();
  void dump(bool rec);
  void reduce2();
  VarCode substCopy(IdentList *actuals);
};

class Code_Project: public Code_c {
public:
  Code_Project(Ident n, VarCode c, Pos p);

  bool equiv(Code&);

  void makeDFA();
  void makeGTA();
  void dump(bool rec);
  void reduce1();
  void reduce2();
  Ident findEquality(Ident x, VarCode vc);
  VarCode substCopy(IdentList *actuals);

  Ident var;
  int   varpos;
};

class Code_Negate: public Code_c {
public:
  Code_Negate(VarCode c, Pos p) :
    Code_c(cNegate, c, p) {}

  void makeDFA();
  void makeGTA();
  void dump(bool rec);
  void reduce1();
  void reduce2();
  VarCode substCopy(IdentList *actuals);
};

class Code_Prefix: public Code_c {
public:
  Code_Prefix(VarCode c, Pos p) :
    Code_c(cPrefix, c, p) {}

  void makeDFA();
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);
};

class Code_And: public Code_cc {
public:
  Code_And(VarCode vc1, VarCode vc2, Pos p) :
    Code_cc(cAnd, vc1, vc2, p) {}

  void makeDFA();
  void makeGTA();
  void dump(bool rec);
  void reduce1();
  void reduce2();
  VarCode substCopy(IdentList *actuals);
};

class Code_IdLeft: public Code_cc {
public:
  Code_IdLeft(VarCode vc1, VarCode vc2, Pos p) :
    Code_cc(cIdLeft, vc1, vc2, p) {}

  void makeDFA();
  void makeGTA();
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);
};

class Code_Or: public Code_cc {
public:
  Code_Or(VarCode vc1, VarCode vc2, Pos p) :
    Code_cc(cOr, vc1, vc2, p) {}

  void makeDFA();
  void makeGTA();
  void dump(bool rec);
  void reduce1();
  VarCode substCopy(IdentList *actuals);
};

class Code_Impl: public Code_cc {
public:
  Code_Impl(VarCode vc1, VarCode vc2, Pos p) :
    Code_cc(cImpl, vc1, vc2, p) {}

  void makeDFA();
  void makeGTA();
  void dump(bool rec);
  void reduce1();
  VarCode substCopy(IdentList *actuals);
};

class Code_Biimpl: public Code_cc {
public:
  Code_Biimpl(VarCode vc1, VarCode vc2, Pos p) :
    Code_cc(cBiimpl, vc1, vc2, p) {}

  void makeDFA();
  void makeGTA();
  void dump(bool rec);
  void reduce1();
  VarCode substCopy(IdentList *actuals);
};

////////// Special nodes //////////////////////////////////////////////////////

class Code_PredCall: public Code_c {
public:
  Code_PredCall(Ident n, VarCode c, IdentList &acts, IdentList *frs, 
		char *source, Pos p);
  ~Code_PredCall() 
  {vc.remove(); delete[] names; delete[] orders; delete[] filename;}

  bool equiv(Code&);
  unsigned hash();
  void makeDFA();
  void makeGTA();
  void dump(bool rec);
  void reduce1();
  void reduce2();
  VarCode substCopy(IdentList *actuals);

  Ident     name;       // predicate name
  char     *filename;   // filename for separate compilation
  char     *sourcefile; // name of source file
  char    **names;      // names of frees+actuals (vars) (NULL terminated)
  char     *orders;     // orders of free+actuals (vars)
  IdentList actuals, frees, s;
};

class Code_Import: public Code {
public:
  Code_Import(char *filename, Deque<char*> *forms, IdentList *acts, Pos p);
  ~Code_Import() {delete formals; delete actuals;}
  
  bool equiv(Code&);
  unsigned hash();
  void makeDFA();
  void makeGTA();
  IdentList *getOffsets(char**, int*, SSSet*);
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);

  char         *file;
  Deque<char*> *formals;
  IdentList    *actuals;
};

class Code_Export: public Code_c {
public:
  Code_Export(VarCode c, char* filename, IdentList *fv, Pos p);
  ~Code_Export() 
  {vc.remove(); delete[] names; delete[] orders;}
  
  bool equiv(Code&);
  unsigned hash();
  void makeDFA();
  void makeGTA();
  void dump(bool rec);
  VarCode substCopy(IdentList *actuals);
  bool checkExport(Ident x);

  char  *file;   // file name
  char **names;  // names of free variables
  char  *orders; // orders of free variables
  int    num;    // number of free variables
  IdentList freevars, s;
};

#endif
