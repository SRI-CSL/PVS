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

#include <iostream.h>
#include "ident.h"
#include "offsets.h"
#include "symboltable.h"

extern SymbolTable symbolTable;
extern Offsets offsets;

int 
varcmp(const void *i1, const void *i2)
{
  if (*((Ident *) i1) < *((Ident *) i2))
    return -1;
  else if (*((Ident *) i1) > *((Ident *) i2))
    return 1;
  else
    return 0;
}

int 
varcmp_offsets(const void *i1, const void *i2)
{
  if (offsets.off(*((Ident *) i1)) < offsets.off(*((Ident *) i2)))
    return -1;
  else if (offsets.off(*((Ident *) i1)) > offsets.off(*((Ident *) i2)))
    return 1;
  else
    return 0;
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

void 
IdentList::dumplist()
{
  cout << "[";
  iterator i;
  for (i = begin(); i != end(); i++) {
    if (i != begin())
      cout << ",";
    cout << *i;
  }
  cout << "]";
}

unsigned
IdentList::hash()
{
  unsigned h = 1;
  for (iterator i = begin(); i != end(); i++) 
    h = (h << 1) + *i;
  return h;
}

void
IdentList::compress()
{ // sort (by offsets!) and remove duplicates
  Deque<Ident>::sort(varcmp_offsets);
  unsigned n = 0;
  for (iterator i = begin(); i != end(); i++)
    if (i+1 == end() || *i != *(i+1))
      set(n++, *i);
  while (n < size())
    pop_back();
}

IdentList *
IdentList::copy()
{
  return (IdentList *) Deque<Ident>::copy();
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
  if (l)
    return new IdentList(*l);
  else
    return NULL;
}

Ident
subst(Ident id, IdentList *i1, IdentList *i2)
{
  invariant(i1 && i2 && i1->size() == i2->size());

  IdentList::iterator p1, p2;
  for (p1 = i1->begin(), p2 = i2->begin(); p1!=i1->end(); p1++, p2++)
    if (*p1==id)
      return *p2;

  /*
  unsigned a = 0, b, c = i1->size();
  while (a < c) {
    b = (c+a)/2;
    Ident t = i1->get(b);
    if (offsets.off(t) > offsets.off(id))
      c = b;
    else if (offsets.off(t) < offsets.off(id))
      a = b+1;
    else
      return i2->get(b);
  }
  */

  return id;
}

Ident
subst(Ident id1, Ident id2, Ident id3)
{
  if (id1 == id2)
    return id3;
  return id1;
}

IdentList *
subst(IdentList *i1, IdentList *i2, IdentList *i3, Ident except)
{ // map i1 using i2->i3 (but leave 'except' unchanged)
  invariant(i1 && i2 && i3 && i2->size() == i3->size());

  IdentList *res = new IdentList;
  for (IdentList::iterator j1 = i1->begin(); j1 != i1->end(); j1++) 
    if (*j1 == except)
      res->push_back(*j1);
    else
      res->push_back(subst(*j1, i2, i3));
  return res;
}

bool
sameUnivs(Ident id1, Ident id2) // test id1's universes same as id2's
{
  return equal(symbolTable.lookupUnivs(id1),
               symbolTable.lookupUnivs(id2));
}

bool
sameListUnivs(IdentList *i1, IdentList *i2)
{
  invariant(i1 && i2 && i1->size() == i2->size());
  
  IdentList::iterator j1,j2;
  for (j1 = i1->begin(), j2 = i2->begin(); j1 != i1->end(); j1++, j2++)
    if (!sameUnivs(*j1, *j2))
      return false;
  return true;
}
