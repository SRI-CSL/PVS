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

#ifndef __IDENT_H
#define __IDENT_H

#include "deque.h"

typedef int Ident;

class IdentList: public Deque<Ident> {
public:
  IdentList() : Deque<Ident>() {}
  IdentList(Ident id) : Deque<Ident>(id) {}

  void insert(Ident id);
  void insert(IdentList *d);
  void remove(Ident id);
  void sort();
  void dump();
  void dumplist();
  unsigned hash();
  void compress();
  IdentList *copy();
};

// operations on sorted IdentLists
bool       equal(IdentList *l1, IdentList *l2);
IdentList *ident_union(IdentList *i1, IdentList *i2); 
IdentList *intersection(IdentList *i1, IdentList *i2);
IdentList *copy(IdentList *l);
Ident      subst(Ident id, IdentList *i1, IdentList *i2);
Ident      subst(Ident id1, Ident id2, Ident id3);
IdentList *subst(IdentList *i1, IdentList *i2, IdentList *i3, 
		 Ident except = -1);
bool       sameUnivs(Ident i1, Ident i2);
bool       sameListUnivs(IdentList *i1, IdentList *i2);

#endif
