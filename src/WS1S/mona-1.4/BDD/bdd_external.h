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

#ifndef __BDD_EXTERNAL_H
#define __BDD_EXTERNAL_H

#include "bdd.h"

typedef struct _BddNode {
  int idx;    
  unsigned lo;
  unsigned hi;
  int p;
} BddNode;

typedef struct _Table {
  BddNode *elms;
  unsigned allocated, noelems;
} Table;

Table *tableInit();
void tableInsert(Table *t, BddNode *elm);
void tableFree(Table *t);

void export(bdd_manager *bddm, unsigned p, Table *table);
unsigned make_node(int i);

#endif
