//
// symboltable.cpp
//
// MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
// 
// Reproduction of all or part of this software is permitted for
// educational or research use on condition that this copyright notice is
// included in any copy. This software comes with no warranty of any
// kind. In no event will BRICS be liable for any damages resulting from
// use of this software.

#include <stdio.h>
#include <string.h>
#include "symboltable.h"
#include "offsets.h"

extern Offsets offsets;
extern void TypeError(String, Pos &);

Ident
SymbolTable::insert(Entry *e)
{
  declarationTable[hash(e->string)].push_back(e);
  identMap.push_back(e);
  offsets.insert();
  return noIdents++;
}

void
SymbolTable::remove(int idx) // must be at end of Deque
{
  declarationTable[idx].pop_back();
  // don't delete and remove from indentMap since the entry
  // might still be in use
}

SymbolTable::Entry&
SymbolTable::lookup(Name *name)
{
  int idx = hash(name->str);

  for (Entry **e = declarationTable[idx].end();
       e != declarationTable[idx].begin(); e--) 
    if ((!(*(e-1))->string && !name->str) ||
	((*(e-1))->string && name->str && 
	 strcmp((*(e-1))->string, name->str) == 0))
      return **(e-1);

  TypeError("'" + String(name->str) + "' not declared", name->pos);
  exit(-1); // avoid compiler warning
}

void
SymbolTable::check(Name *name)
{
  int idx = hash(name->str);
  
  for (Entry **e = declarationTable[idx].begin();
       e != declarationTable[idx].end(); e++) 
    if ((*e)->string && name->str && strcmp((*e)->string, name->str) == 0)
      TypeError("'" + String(name->str) + "' already declared", name->pos);
}

int
SymbolTable::hash(char *s) 
{
  return (int) s % size;
}

SymbolTable::SymbolTable(int s)
{
  size = s;
  noIdents = 0;
  defaultRestriction1 = NULL;
  defaultRestriction2 = NULL;
  declarationTable = new Deque<Entry*>[size];
  symbols = new Deque<char*>[size];
}

SymbolTable::~SymbolTable()
{
  delete[] declarationTable;

  Deque<Entry*>::iterator l;
  for (l = identMap.begin(); 
       l != identMap.end(); l++)
    delete *l;
  
  unsigned i;
  Deque<char*>::iterator j;
  for (i = 0; i < size; i++)
    for (j = symbols[i].begin(); j != symbols[i].end(); j++)
      delete[] *j;
  delete[] symbols;
}


char *
SymbolTable::insertString(char *str)
{
  char *t = str;
  unsigned idx = 0;
  while (*t)
    idx = (idx << 1) + *t++;
  idx = idx % size;

  char **s;
  for (s = symbols[idx].begin(); s != symbols[idx].end(); s++)
    if (strcmp(str, *s) == 0) {
      delete[] str; // string used before
      return *s;
    } 

  symbols[idx].push_back(str); // string never used before
  return str; 
}

Ident
SymbolTable::insertPred(Name *name)
{
  check(name);

  return insert(new PredEntry(name->str, noIdents));
}

Ident 
SymbolTable::insertVar(Name *name, MonaTypeTag type, 
		       IdentList *univs, bool local, bool fresh)
{
  if (local)
    localStack.push_back(hash(name->str));
  else
    check(name);

  return insert(new VarEntry(name->str, type, noIdents, univs, fresh));
}

Ident 
SymbolTable::insertUniv(Name *name, char *pos, bool dummy)
{
  allUnivIds.push_back(noIdents); // automatically sorted
  if (!dummy)
    allRealUnivIds.push_back(noIdents); // automatically sorted
  check(name);
  return insert(new UnivEntry(name->str, noIdents, allUnivIds.size()-1, pos));
}

Ident 
SymbolTable::insertConst(Name *name, int value)
{
  check(name);
  return insert(new ConstEntry(name->str, noIdents, value));
}

Ident 
SymbolTable::insertStatespace(Name *name, int number)
{
  check(name);
  return insert(new StateSpaceEntry(name->str, noIdents, number));
}

Ident
SymbolTable::insertFresh(MonaTypeTag type, IdentList *univs)
{
  Name dummy = Name(NULL, dummyPos);
  return insertVar(&dummy, type, univs, false, true);
}

void 
SymbolTable::openLocal()
{
  localStack.push_back(-1);
}

void 
SymbolTable::closeLocal()
{
  int i;
  while ((i = localStack.pop_back()) != -1)
    remove(i);
}

Ident
SymbolTable::lookupIdent(Name *name)
{
  return lookup(name).ident;
}

char*
SymbolTable::lookupSymbol(Ident ident)
{
  char *res = (identMap.get(ident))->string;
  if (!res)
    res = "<fresh>";
  return res;
}

MonaTypeTag
SymbolTable::lookupType(Ident ident)
{
  return (identMap.get(ident))->monaTypeTag;
}

MonaTypeTag
SymbolTable::lookupType(Name *name)
{
  return lookup(name).monaTypeTag;
}

int
SymbolTable::lookupOrder(Ident ident)
{
  switch ((identMap.get(ident))->monaTypeTag) {
  case Varname0:
  case Parname0:
    return 0;
  case Varname1:
  case Parname1:
    return 1;
  case Varname2:
  case Parname2:
    return 2;
  default:
    return -1;
  }
}

int
SymbolTable::lookupValue(Name *name)
{
  return ((ConstEntry &) lookup(name)).value;
}

int
SymbolTable::lookupNumber(Name *name)
{
  return ((StateSpaceEntry &) lookup(name)).number;
}

int
SymbolTable::lookupNumber(Ident ident)
{
  return ((StateSpaceEntry *) identMap.get(ident))->number;
}

int
SymbolTable::lookupUnivNumber(Ident ident)
{
  return ((UnivEntry *) identMap.get(ident))->number;
}

char*
SymbolTable::lookupPos(Ident ident)
{
  return ((UnivEntry *) identMap.get(ident))->pos;
}

IdentList*  
SymbolTable::lookupUnivIdents(NameList &names)
{
  IdentList *idents = new IdentList;

  NameList::iterator i;

  for (i = names.begin(); i != names.end(); i++) {
    Entry &e = lookup(*i);

    switch (e.monaTypeTag) {

    case Univname:
    case Varname1:
    case Varname2:
    case Parname1:
    case Parname2:
    case ParnameU:
      idents->push_back(e.ident); // vars are substituted later
      break;

    default:
      TypeError("'" + String((*i)->str) + 
		"' is not a universe or a first or second order variable", 
		(*i)->pos);
    }
  }

  return idents;
}

IdentList*
SymbolTable::lookupUnivs(Ident ident)
{
  return ((VarEntry *) identMap.get(ident))->universes;
}

ASTForm*
SymbolTable::lookupRestriction(Ident ident)
{
  return ((VarEntry *) identMap.get(ident))->restriction;
}

ASTForm*
SymbolTable::getRestriction(Ident ident, Ident *formal)
{
  VarEntry *e = (VarEntry *) identMap.get(ident);
  ASTForm *f;
  switch (e->monaTypeTag) {
  case Varname0:
  case Varname1:
  case Varname2:
  case Parname0:
  case Parname1:
  case Parname2: 
  case ParnameU:
    *formal = -1; // assume default restriction not needed
    f = e->restriction;
    if (!f && !e->fresh && e->string) {
      // return default restriction for non-fresh variables
      switch (e->monaTypeTag) {
      case Parname1:
      case Varname1:
	f = defaultRestriction1;
	*formal = defaultIdent1;
	break;
      case Parname2:
      case Varname2:
	f = defaultRestriction2;
	*formal = defaultIdent2;
	break;
      default: ; // should not occur
      }
    }
    break;
  default: // the rest can never have restrictions
    f = 0;
    break;
  }
  return f;
}

ASTForm*
SymbolTable::getDefault1Restriction(Ident *formal)
{
  *formal = defaultIdent1;
  return defaultRestriction1;
}

ASTForm*
SymbolTable::getDefault2Restriction(Ident *formal)
{
  *formal = defaultIdent2;
  return defaultRestriction2;
}

IdentList*  
SymbolTable::allUnivs()
{
  if (allUnivIds.empty())
    return 0;
  else
    return new IdentList(allUnivIds);
}

IdentList*  
SymbolTable::allRealUnivs()
{
  if (allRealUnivIds.empty())
    return 0;
  else
    return new IdentList(allRealUnivIds);
}

void
SymbolTable::updateUnivs(Ident ident, IdentList *univs)
{
  VarEntry *e = (VarEntry *) identMap.get(ident);
  delete e->universes;
  e->universes = univs;
}

void
SymbolTable::updateRestriction(Ident ident, ASTForm *restriction)
{
  ((VarEntry *) identMap.get(ident))->restriction = restriction;
  // (don't delete old restriction)
}

void 
SymbolTable::checkUniqueness(NameList &names)
{
  NameList::iterator i, j;

  for (i = names.begin(); i != names.end(); i++)
    for (j = names.begin(); j != i; j++)
      if ((*i)->str && (*j)->str && strcmp((*i)->str, (*j)->str) == 0)
	TypeError("'" + String((*i)->str) + 
		  "' already declared", (*i)->pos);
}

void
SymbolTable::setDefaultRestriction(MonaTypeTag type, ASTForm *f, Ident id)
{
  switch (type) {
  case Varname1:
    defaultRestriction1 = f;
    defaultIdent1 = id;
    break;
  case Varname2:
    defaultRestriction2 = f;
    defaultIdent2 = id;
    break;
  default: ;
  }
}

void
SymbolTable::dump()
{
  cout << "\nSymbol table:\n" 
       << "id     offset  name                 type\n"
       << "----------------------------------------------\n";

  Ident id;
  for (id = 0; id < (signed) noIdents; id++) {
    Entry *e = identMap.get(id);
    char *type, *str;

    switch (e->monaTypeTag) {
    case Varname0:
      type = "var0";
      break;
    case Varname1: 
      type = "var1";
      break;
    case Varname2:
      type = "var2";
      break;
    case Parname0: 
      type = "par0";
      break;
    case Parname1: 
      type = "par1";
      break;
    case Parname2: 
      type = "par2";
      break;
    case ParnameU:
      type = "par univ";
      break;
    case Univname: 
      type = "univ";
      break;
    case Predname:
      type = "pred/macro";
      break;
    case Constname: 
      type = "const";
      break;
    case Statespacename:
      type = "statespace";
      break;
    default:
      invariant(false);
    }

    str = e->string;
    if (!str)
      str = "<fresh>";

    printf("%-5i  #%-5i  %-20s %s\n", id, offsets.off(id), str, type);
  }
}
