//
// ident.cpp
//
// MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
// 
// Reproduction of all or part of this software is permitted for
// educational or research use on condition that this copyright notice is
// included in any copy. This software comes with no warranty of any
// kind. In no event will BRICS be liable for any damages resulting from
// use of this software.

#include <iostream.h>
#include "ident.h"
#include "offsets.h"
#include "symboltable.h"

extern Offsets offsets;
extern SymbolTable symbolTable;

int 
varcmp(const void *i1, const void *i2)
{
  if (offsets.off(*((Ident *) i1)) < offsets.off(*((Ident *) i2)))
    return -1;
  else if (offsets.off(*((Ident *) i1)) > offsets.off(*((Ident *) i2)))
    return 1;
  else
    return 0;
}

IdentList::IdentList(const IdentList &list) : 
  Deque<Ident>()
{
  for (iterator i = list.begin(); i != list.end(); i++)
    push_back(*i);
}

void 
IdentList::insert(Ident id)
{
  for (iterator i = begin(); i != end(); i++)
    if (*i == id)
      return;
  push_back(id);
}

void 
IdentList::insert(IdentList *d)
{
  for (iterator i = d->begin(); i != d->end(); i++)
    insert(*i);
}

bool 
IdentList::exists(Ident id)
{
  for (iterator i = begin(); i != end(); i++)
    if (*i == id)
      return true;
  return false;
}

void
IdentList::remove(Ident id)
{
  for (iterator i = begin(); i != end(); i++)
    if (*i == id) {
      Ident f = pop_front();
      if (id != f)
	set(i, f);
      return;
    }
}

void 
IdentList::sort()
{
  Deque<Ident>::sort(varcmp);
}

void 
IdentList::dump()
{
  for (iterator i = begin(); i != end();) {
    cout << symbolTable.lookupSymbol(*i);
    if (++i != end())
      cout << ",";
  }
}

unsigned
IdentList::hash()
{
  unsigned h = 1;
  for (iterator i = begin(); i != end(); i++) 
    h = (h << 1) + *i;
  return h;
}

bool
equal(IdentList *l1, IdentList *l2)
{
  if (l1 == NULL && l2 == NULL)
    return true;

  if (l1 == NULL || l2 == NULL)
    return false;

  if (l1->size() != l2->size())
    return false;

  for (IdentList::iterator i = l1->begin(), j = l2->begin();
       i != l1->end(); i++, j++)
    if (*i != *j)
      return false;

  return true;
}

IdentList*
ident_union(IdentList *i1, IdentList *i2) 
  // merge sorted identlists into union
{
  if ((!i1 || i1->empty()) && (!i2 || i2->empty()))
    return NULL;
  if (!i1)
    return new IdentList(*i2);
  if (!i2)
    return new IdentList(*i1);

  IdentList *r = new IdentList;

  Ident *id1 = i1->begin();
  Ident *id2 = i2->begin();
  
  while (id1 != i1->end() && id2 != i2->end()) {
    if (*id1 < *id2) {
      r->push_back(*id1);
      id1++;
    }
    else if (*id1 > *id2) {
      r->push_back(*id2);
      id2++;
    }
    else {
      r->push_back(*id1);
      id1++;
      id2++;
    }
  }
  while (id1 != i1->end()) {
    r->push_back(*id1);
    id1++;
  }
  while (id2 != i2->end()) {
    r->push_back(*id2);
    id2++;
  }

  return r;
}

IdentList*
intersection(IdentList *i1, IdentList *i2)
  // make sorted intersection of sorted identlists
{
  if (!i1 || !i2)
    return NULL;

  IdentList *r = new IdentList;

  Ident *id1 = i1->begin();
  Ident *id2 = i2->begin();
  
  while (id1 != i1->end() && id2 != i2->end()) {
    if (*id1 < *id2) 
      id1++;
    else if (*id1 > *id2) 
      id2++;
    else {
      r->push_back(*id1);
      id1++;
      id2++;
    }
  }

  if (r->empty()) {
    delete r;
    r = NULL;
  }
  return r;
}

IdentList*
copy(IdentList *l)
{
  if (l && !l->empty())
    return new IdentList(*l);
  else
    return NULL;
}
