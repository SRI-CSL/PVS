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

#ifndef __PREDLIB_H
#define __PREDLIB_H

#include "ast.h"
#include "signature.h"
#include "st_dfa.h"
#include "st_gta.h"
#include "deque.h"

class PredLibEntry {
public:
  PredLibEntry(IdentList *eFormals, IdentList *eFrees, IdentList *eBound,
	       ASTForm *eFormula, bool eIsMacro, int eName, char *eSource) :
    formals(eFormals), frees(eFrees), bound(eBound), ast(eFormula),
    isMacro(eIsMacro), name(eName), source(eSource) {}
  ~PredLibEntry() 
  {delete formals; delete frees; delete bound; delete ast;}

  IdentList *formals;
  IdentList *frees;
  IdentList *bound;
  ASTForm   *ast;
  bool       isMacro;
  Ident      name;
  char      *source;
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
  
  void          insert(IdentList *formals, IdentList *frees, IdentList *bound,
		       ASTForm *formula, bool isMacro, int name, char *source);
  PredLibEntry *lookup(Ident);
  TestResult    testTypes(Ident name, ASTList *acts, int *no = NULL);
  PredLibEntry *first();
  PredLibEntry *next();
};

#endif
