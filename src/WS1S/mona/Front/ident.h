//
// ident.h
//
// MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
// 
// Reproduction of all or part of this software is permitted for
// educational or research use on condition that this copyright notice is
// included in any copy. This software comes with no warranty of any
// kind. In no event will BRICS be liable for any damages resulting from
// use of this software.

#ifndef __IDENT_H
#define __IDENT_H

#include "deque.h"

typedef int Ident;

class IdentList: public Deque<Ident> {
public:
  IdentList() : Deque<Ident>() {}
  IdentList(Ident id) : Deque<Ident>(id) {}
  IdentList(const IdentList &list);

  void insert(Ident id);
  void insert(IdentList *d);
  bool exists(Ident id);
  void remove(Ident id);
  void sort();
  void dump();
  unsigned hash();
};

// operations on sorted IdentLists
bool       equal(IdentList *l1, IdentList *l2);
IdentList *ident_union(IdentList *i1, IdentList *i2); 
IdentList *intersection(IdentList *i1, IdentList *i2);
IdentList *copy(IdentList *l);

#endif
