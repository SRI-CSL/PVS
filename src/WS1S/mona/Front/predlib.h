//
// predlib.h
//
// MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
// 
// Reproduction of all or part of this software is permitted for
// educational or research use on condition that this copyright notice is
// included in any copy. This software comes with no warranty of any
// kind. In no event will BRICS be liable for any damages resulting from
// use of this software.

#ifndef __PREDLIB_H
#define __PREDLIB_H

#include "ast.h"
#include "signature.h"
#include "st_dfa.h"
#include "st_gta.h"
#include "deque.h"

class PredLibEntry {
public:
  PredLibEntry(IdentList    *eFormals, 
	       IdentList    *eFrees,
	       IdentList    *eBound,
	       PragmaList   *eLocalPragmas,
	       ASTForm      *eFormula,
	       bool          eIsMacro,
	       int           eName,
	       char         *eSource) :
    formals(eFormals),
    frees(eFrees),
    bound(eBound),
    localPragmas(eLocalPragmas),
    ast(eFormula),
    isMacro(eIsMacro),
    name(eName),
    source(eSource) {}
  ~PredLibEntry() 
  {
    delete formals; delete frees; delete bound; 
    delete localPragmas; delete ast;
  }

  IdentList    *formals;
  IdentList    *frees;
  IdentList    *bound;
  PragmaList   *localPragmas;
  ASTForm      *ast;
  bool          isMacro;
  Ident         name;
  char         *source;
};

enum TestResult {
  tOK,
  tWrongNoParameters,
  tWrongParameterType
};

#define PREDLIB_SIZE 113

class PredicateLib {
  Deque<PredLibEntry*> *table; // hashtable Ident -> PredLibEntry

  void insert(PredLibEntry *);

  int            idx;
  PredLibEntry **current;

public:
  PredicateLib();
  ~PredicateLib();
  
  void          insert(IdentList    *formals, 
		       IdentList    *frees,
		       IdentList    *bound,
		       PragmaList   *localPragmas,
		       ASTForm      *formula,
		       bool          isMacro,
		       int           name,
		       char         *source);
  PredLibEntry *lookup(Ident);
  TestResult    testTypes(Ident name, ASTList *acts, int *no = NULL);
  PredLibEntry *first();
  PredLibEntry *next();
};

#endif
