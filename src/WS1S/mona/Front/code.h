//
// code.h
//
// MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
// 
// Reproduction of all or part of this software is permitted for
// educational or research use on condition that this copyright notice is
// included in any copy. This software comes with no warranty of any
// kind. In no event will BRICS be liable for any damages resulting from
// use of this software.

#ifndef __CODE_H
#define __CODE_H

#include "deque.h"
#include "signature.h"
#include "st_dfa.h"
#include "st_gta.h"
#include "printline.h"
#include "ident.h"
#include "string.h"

SSSet stateSpaces(IdentList *univs);
SSSet stateSpaces(Ident id);

////////// Substitution ///////////////////////////////////////////////////////

struct Subst { // a substitution array is terminated with .formal=-1
  Ident formal;
  Ident actual;
}; // (used for dumping DAG contents)

////////// Code ///////////////////////////////////////////////////////////////

enum CodeKind {
  cTrue, 
  cFalse,
  cIn,
  cEq1,
  cEq2,
  cSub2,
  cLess1,
  cLessEq1,
  cEqUnion,
  cEqInter,
  cEqSetMinus,
  cEqEmpty,
  cEqRoot,
  cFirstOrder,
  cEqConst,
  cSingleton,
  cBoolVar,
  cPredCall,
  cEqDot0,
  cEqDot1,
  cEqUp,
  cEqPlus1,
  cEqMinus1,
  cEqPlus2,
  cEqMinus2,
  cEqPlusModulo,
  cEqMinusModulo,
  cEqMin,
  cEqMax,
  cAnd,
  cOr,
  cImpl,
  cBiimpl,
  cRestrict,
  cProject,
  cNegate,
  cExternal,
  cImport,
  cExport,
  cPrefix,
  cInStateSpace,
  cIdLeft,
  cEqPresbConst
}; // must be same order as in Code::viz()

class Code {
public:
  Code(CodeKind knd, Pos p) :
    vars(NULL), size(0), sign(NULL), small(true), 
    kind(knd), refs(1), dfa(NULL), pos(p), mark(false) {}
  virtual ~Code() {delete sign;}

  // determine syntax/signature equivalence
  virtual bool equiv(Code &c); 

  // hash node contents for CodeTable
  virtual unsigned hash(); 

  // generate DFA/GTA recursively with free variables substituted by 'free'
  DFA *DFATranslate(IdentList *free); 
  GTA *GTATranslate(IdentList *free); 

  // make DFA/GTA for this node
  virtual void makeDFA() {}
  virtual void makeGTA() {}

  // remove one reference to this node, if last then call recursively
  virtual void remove();

  // dump node/subtree contents
  virtual void viz(); // graphviz format
  virtual void dump(IdentList *free, Subst *subst) = 0;
  virtual void dump() {}

  // write error message
  void error(String);

  IdentList *vars;  // default free variables, NULL if empty or not small 
  unsigned   size;  // number of free variables (incl. multiple occurences)
  Signature *sign;  // signature of free variables, NULL if empty or not small
  bool       small; // signature equivalence if small, syntax otherwise
  CodeKind   kind;  // object kind tag
  int        refs;  // number of DAG references to this node
  Pos        pos;   // source position
  bool       mark;  // used for traversal
  union {
    DFA *dfa; // default DFA, NULL if not generated yet
    GTA *gta; // default GTA, NULL if not generated yet
  };
};

class VarCode {
public:
  VarCode(IdentList *v, Code *c) :
    vars(v), code(c) {}
  VarCode() {}

  // remove sub-node, use bottom-up when node no longer needed
  void detach();

  IdentList *vars; // overriding free variables, NULL if no override
  Code      *code; // code node
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
  ~Code_c() {vc.detach();}

  bool equiv(Code&);
  unsigned hash();
  void remove();
  void viz();

  VarCode vc;
};

class Code_cc: public Code {
public:
  Code_cc(CodeKind knd, VarCode c1, VarCode c2, Pos p);
  ~Code_cc() {vc1.detach(); vc2.detach();}

  bool equiv(Code&);
  unsigned hash();
  void split(IdentList*, Subst*);
  void remove();
  void viz();

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
  void dump(IdentList*, Subst*);
  void dump();
};

class Code_False: public Code {
public:
  Code_False(Pos p) :
    Code(cFalse, p) {}

  void makeDFA();
  void makeGTA();
  void dump(IdentList*, Subst*);
  void dump();
};


class Code_EqEmpty: public Code_n {
public:
  Code_EqEmpty(Ident i, Pos p) :
    Code_n(cEqEmpty, i, p) {}

  void makeDFA();
  void makeGTA();
  void dump(IdentList*, Subst*);
  void dump();
};

class Code_EqRoot: public Code_n {
public:
  Code_EqRoot(Ident i, IdentList *univs, Pos p) :
    Code_n(cEqRoot, i, p), universes(univs) {}
  ~Code_EqRoot() {delete universes;}

  bool equiv(Code&);

  void makeGTA();
  void dump(IdentList*, Subst*);
  void dump();

  IdentList *universes;
};

class Code_FirstOrder: public Code_n {
public:
  Code_FirstOrder(Ident i, Pos p) :
    Code_n(cFirstOrder, i, p) {}

  void makeDFA();
  void makeGTA();
  void dump(IdentList*, Subst*);
  void dump();
};

class Code_EqConst: public Code_ni {
public:
  Code_EqConst(Ident i, int n, Pos p) :
    Code_ni(cEqConst, i, n, p) {}

  void makeDFA();
  void dump(IdentList*, Subst*);
  void dump();
};

class Code_Singleton: public Code_n {
public:
  Code_Singleton(Ident i, Pos p) :
    Code_n(cSingleton, i, p) {}

  void makeDFA();
  void makeGTA();
  void dump(IdentList*, Subst*);
  void dump();
};

class Code_BoolVar: public Code_n {
public:
  Code_BoolVar(Ident i, Pos p) :
    Code_n(cBoolVar, i, p) {}

  void makeDFA();
  void makeGTA();
  void dump(IdentList*, Subst*);
  void dump();
};

class Code_InStateSpace: public Code_n {
public:
  Code_InStateSpace(Ident i, IdentList *s, Pos p) :
    Code_n(cInStateSpace, i, p), ss(s) {}
  ~Code_InStateSpace() {delete ss;}

  bool equiv(Code&);
  unsigned hash();

  void makeGTA();
  void dump(IdentList*, Subst*);
  void dump();

  IdentList *ss;
};

class Code_In: public Code_nn {
public:
  Code_In(Ident i, Ident j, Pos p) :
    Code_nn(cIn, i, j, p) {}

  void makeDFA();
  void makeGTA();
  void dump(IdentList*, Subst*);
  void dump();
};


class Code_Eq1: public Code_nn {
public:
  Code_Eq1(Ident i, Ident j, Pos p) :
    Code_nn(cEq1, i, j, p) {}

  void makeDFA();
  void makeGTA();
  void dump(IdentList*, Subst*);
  void dump();
};

class Code_Eq2: public Code_nn {
public:
  Code_Eq2(Ident i, Ident j, Pos p) :
    Code_nn(cEq2, i, j, p) {}

  void makeDFA();
  void makeGTA();
  void dump(IdentList*, Subst*);
  void dump();
};

class Code_Sub2: public Code_nn {
public:
  Code_Sub2(Ident i, Ident j, Pos p) :
    Code_nn(cSub2, i, j, p) {}

  void makeDFA();
  void makeGTA();
  void dump(IdentList*, Subst*);
  void dump();
};

class Code_Less1: public Code_nn {
public:
  Code_Less1(Ident i, Ident j, Pos p) :
    Code_nn(cLess1, i, j, p) {}

  void makeDFA();
  void makeGTA();
  void dump(IdentList*, Subst*);
  void dump();
};

class Code_LessEq1: public Code_nn {
public:
  Code_LessEq1(Ident i, Ident j, Pos p) :
    Code_nn(cLessEq1, i, j, p) {}

  void makeDFA();
  void makeGTA();
  void dump(IdentList*, Subst*);
  void dump();
};

class Code_EqDot0: public Code_nn {
public:
  Code_EqDot0(Ident i, Ident j, Pos p) :
    Code_nn(cEqDot0, i, j, p) {}

  void makeGTA();
  void dump(IdentList*, Subst*);
  void dump();
};

class Code_EqDot1: public Code_nn {
public:
  Code_EqDot1(Ident i, Ident j, Pos p) :
    Code_nn(cEqDot1, i, j, p) {}

  void makeGTA();
  void dump(IdentList*, Subst*);
  void dump();
};

class Code_EqUp: public Code_nn {
public:
  Code_EqUp(Ident i, Ident j, Pos p) :
    Code_nn(cEqUp, i, j, p) {}

  void makeGTA();
  void dump(IdentList*, Subst*);
  void dump();
};

class Code_EqPlus2: public Code_nn {
public:
  Code_EqPlus2(Ident i, Ident j, Pos p) :
    Code_nn(cEqPlus2, i, j, p) {}

  void makeDFA();
  void dump(IdentList*, Subst*);
  void dump();
};

class Code_EqMinus2: public Code_nn {
public:
  Code_EqMinus2(Ident i, Ident j, Pos p) :
    Code_nn(cEqMinus2, i, j, p) {}

  void makeDFA();
  void dump(IdentList*, Subst*);
  void dump();
};

class Code_EqMin: public Code_nn {
public:
  Code_EqMin(Ident i, Ident j, Pos p) :
    Code_nn(cEqMin, i, j, p) {}

  void makeDFA();
  void dump(IdentList*, Subst*);
  void dump();
};

class Code_EqMax: public Code_nn {
public:
  Code_EqMax(Ident i, Ident j, Pos p) :
    Code_nn(cEqMax, i, j, p) {}

  void makeDFA();
  void dump(IdentList*, Subst*);
  void dump();
};

class Code_EqPlus1: public Code_nni {
public:
  Code_EqPlus1(Ident i, Ident j, int n, Pos p) :
    Code_nni(cEqPlus1, i, j, n, p) {}

  void makeDFA();
  void dump(IdentList*, Subst*);
  void dump();
};

class Code_EqMinus1: public Code_nni {
public:
  Code_EqMinus1(Ident i, Ident j, int n, Pos p) :
    Code_nni(cEqMinus1, i, j, n, p) {}

  void makeDFA();
  void dump(IdentList*, Subst*);
  void dump();
};

class Code_EqUnion: public Code_nnn {
public:
  Code_EqUnion(Ident i, Ident j, Ident k, Pos p) :
    Code_nnn(cEqUnion, i, j, k, p) {}

  void makeDFA();
  void makeGTA();
  void dump(IdentList*, Subst*);
  void dump();
};

class Code_EqInter: public Code_nnn {
public:
  Code_EqInter(Ident i, Ident j, Ident k, Pos p) :
    Code_nnn(cEqInter, i, j, k, p) {}

  void makeDFA();
  void makeGTA();
  void dump(IdentList*, Subst*);
  void dump();
};

class Code_EqSetMinus: public Code_nnn {
public:
  Code_EqSetMinus(Ident i, Ident j, Ident k, Pos p) :
    Code_nnn(cEqSetMinus, i, j, k, p) {}

  void makeDFA();
  void makeGTA();
  void dump(IdentList*, Subst*);
  void dump();
};

class Code_EqPlusModulo: public Code_nnn {
public:
  Code_EqPlusModulo(Ident i, Ident j, Ident k, Pos p) :
    Code_nnn(cEqPlusModulo, i, j, k, p) {}

  void makeDFA();
  void dump(IdentList*, Subst*);
  void dump();
};

class Code_EqMinusModulo: public Code_nnn {
public:
  Code_EqMinusModulo(Ident i, Ident j, Ident k, Pos p) :
    Code_nnn(cEqMinusModulo, i, j, k, p) {}

  void makeDFA();
  void dump(IdentList*, Subst*);
  void dump();
};

class Code_EqPresbConst: public Code_ni {
public:
  Code_EqPresbConst(Ident i, int v, Pos p) :
    Code_ni(cEqPresbConst, i, v, p) {}

  void makeDFA();
  void dump(IdentList*, Subst*);
  void dump();
};

////////// Composite formulas /////////////////////////////////////////////////

class Code_Restrict: public Code_c {
public:
  Code_Restrict(VarCode vc, Pos p) :
    Code_c(cRestrict, vc, p) {}

  void makeDFA();
  void makeGTA();
  void dump(IdentList*, Subst*);
};

class Code_Project: public Code_c {
public:
  Code_Project(Ident n, VarCode c, Pos p);

  bool equiv(Code&);

  void makeDFA();
  void makeGTA();
  void dump(IdentList*, Subst*);

  Ident var;
  int   varpos;
};

class Code_Negate: public Code_c {
public:
  Code_Negate(VarCode c, Pos p) :
    Code_c(cNegate, c, p) {}

  void makeDFA();
  void makeGTA();
  void dump(IdentList*, Subst*);
};

class Code_Prefix: public Code_c {
public:
  Code_Prefix(VarCode c, Pos p) :
    Code_c(cPrefix, c, p) {}

  void makeDFA();
  void dump(IdentList*, Subst*);
};

class Code_And: public Code_cc {
public:
  Code_And(VarCode vc1, VarCode vc2, Pos p) :
    Code_cc(cAnd, vc1, vc2, p) {}

  void makeDFA();
  void makeGTA();
  void dump(IdentList*, Subst*);
};

class Code_IdLeft: public Code_cc {
public:
  Code_IdLeft(VarCode vc1, VarCode vc2, Pos p) :
    Code_cc(cIdLeft, vc1, vc2, p) {}

  void makeDFA();
  void makeGTA();
  void dump(IdentList*, Subst*);
};

class Code_Or: public Code_cc {
public:
  Code_Or(VarCode vc1, VarCode vc2, Pos p) :
    Code_cc(cOr, vc1, vc2, p) {}

  void makeDFA();
  void makeGTA();
  void dump(IdentList*, Subst*);
};

class Code_Impl: public Code_cc {
public:
  Code_Impl(VarCode vc1, VarCode vc2, Pos p) :
    Code_cc(cImpl, vc1, vc2, p) {}

  void makeDFA();
  void makeGTA();
  void dump(IdentList*, Subst*);
};

class Code_Biimpl: public Code_cc {
public:
  Code_Biimpl(VarCode vc1, VarCode vc2, Pos p) :
    Code_cc(cBiimpl, vc1, vc2, p) {}

  void makeDFA();
  void makeGTA();
  void dump(IdentList*, Subst*);
};

////////// Special nodes //////////////////////////////////////////////////////

class Code_PredCall: public Code_c {
public:
  Code_PredCall(Ident n, VarCode c, IdentList &acts, IdentList *frees, 
		char *filename, Pos p);
  ~Code_PredCall() 
  {vc.detach(); delete[] names; delete s; delete[] orders;}

  bool equiv(Code&);
  unsigned hash();

  void makeDFA();
  void makeGTA();
  void dump(IdentList*, Subst*);

  Ident      name;   // predicate name
  IdentList *s;      // mirror of sign
  char      *file;   // filename for separate compilation
  char     **names;  // names of frees+actuals (vars) (NULL terminated)
  int       *orders; // orders of free+actuals (vars)
};

class Code_External: public Code {
public:
  Code_External(char *filename, IdentList &acts, IdentList *frees, Pos p);
  ~Code_External() {delete s;}
  
  bool equiv(Code&);
  unsigned hash();

  void makeDFA();
  void makeGTA();
  void dump(IdentList*, Subst*);

  IdentList *s;    // mirror of sign
  char      *file; // filename
};

class Code_Import: public Code {
public:
  Code_Import(char *filename, Deque<char*> *forms, IdentList *acts, Pos p) :
    Code(cImport, p), file(filename), formals(forms), actuals(acts) {}
  ~Code_Import() {delete formals; delete actuals;}
  
  bool equiv(Code&);
  unsigned hash();

  void makeDFA();
  void makeGTA();
  IdentList *getOffsets(char**, int*, SSSet*);
  void dump(IdentList*, Subst*);

  char         *file;
  Deque<char*> *formals;
  IdentList    *actuals;
};

class Code_Export: public Code_c {
public:
  Code_Export(VarCode c, IdentList *freevars, char* filename, Pos p);
  ~Code_Export() 
  {vc.detach(); delete[] names; delete frees; delete s; delete[] orders;}
  
  bool equiv(Code&);
  unsigned hash();

  void makeDFA();
  void makeGTA();
  void dump(IdentList*, Subst*);

  IdentList *frees;  // free variables
  char      *file;
  IdentList *s;      // signature of free variables
  char     **names;  // names of free variables
  int       *orders; // orders of free variables
};

////////// CodeTable //////////////////////////////////////////////////////////

#define CODE_TABLE_SIZE 1019

class CodeTable {
  Deque<Code*> table[CODE_TABLE_SIZE];

public:
  CodeTable() {stat_hits = stat_misses = nodes = 0;}

  VarCode insert(Code*); 
  VarCode insert(VarCode); 
  bool    exists(Code&);
  void    remove(Code*);
  void    reset();
  void    print_statistics();
  void    print_sizes();

  int stat_hits, stat_misses, nodes;
};

#endif
