//
// predlib.cpp
//
// MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
// 
// Reproduction of all or part of this software is permitted for
// educational or research use on condition that this copyright notice is
// included in any copy. This software comes with no warranty of any
// kind. In no event will BRICS be liable for any damages resulting from
// use of this software.

#include "env.h"
#include "offsets.h"
#include "predlib.h"
#include "symboltable.h"

extern Environment environment;
extern Offsets     offsets;
extern SymbolTable symbolTable;

void
PredicateLib::insert(PredLibEntry *p)
{
  unsigned i = p->name % PREDLIB_SIZE;
  
  table[i].push_back(p);
}

PredicateLib::PredicateLib()
{
  table = new Deque<PredLibEntry*>[PREDLIB_SIZE];
}

PredicateLib::~PredicateLib()
{
  int i;
  Deque<PredLibEntry*>::iterator j;
  for (i = 0; i < PREDLIB_SIZE; i++)
    for (j = table[i].begin(); j != table[i].end(); j++)
      delete *j;
  delete[] table;
}

void 
PredicateLib::insert(IdentList    *formals, 
		     IdentList    *frees,
		     IdentList    *bound,
		     PragmaList   *localPragmas,
		     ASTForm      *formula,
		     bool          isMacro,
		     int           name,
		     char         *source)
{
  insert(new PredLibEntry(formals, frees, bound, localPragmas, 
			  formula, isMacro, name, source));
}

PredLibEntry *
PredicateLib::lookup(Ident id)
{
  unsigned i = id % PREDLIB_SIZE;

  PredLibEntry **pp;
  for (pp = table[i].begin(); pp != table[i].end(); pp++)
    if ((*pp)->name == id)
      return *pp;

  invariant(false);
  return NULL;
}

TestResult
PredicateLib::testTypes(Ident name, ASTList *acts, int *no) 
{ 
  PredLibEntry *entry = lookup(name);

  if (entry->formals->size() != acts->size())
    return tWrongNoParameters; 
  
  ASTList::iterator a;
  IdentList::iterator f;
  int i;
  for (f = entry->formals->begin(), a = acts->begin(), i = 1;
       f != entry->formals->end();
       f++, a++, i++) {
  
    if (no)
      *no = i;

    MonaTypeTag t = symbolTable.lookupType(*f);

    switch ((*a)->order) {

    case oTerm1:
      if (t != Parname1 && t != ParnameU)
	return tWrongParameterType;
      break;

    case oTerm2:
      if (t != Parname2 && t != ParnameU)
	return tWrongParameterType;
      break;

    case oForm:
      if (t != Parname0)
	return tWrongParameterType;
      break;

    case oUniv:
      if (t != ParnameU)
	return tWrongParameterType;
      break;
    }
  } 

  return tOK;
}

PredLibEntry*
PredicateLib::first()
{
  idx = -1;

  while (++idx < PREDLIB_SIZE)
    if ((current = table[idx].begin()) != table[idx].end())
      return *current;

  return NULL;
}

PredLibEntry*
PredicateLib::next()
{
  if (++current != table[idx].end())
    return *current;

  while (++idx < PREDLIB_SIZE)
    if ((current = table[idx].begin()) != table[idx].end())
      return *current;

  return NULL;
}
