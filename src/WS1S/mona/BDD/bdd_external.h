/* bdd_external.h */

/*
 * MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
 *
 * Reproduction of all or part of this software is permitted for
 * educational or research use on condition that this copyright notice is
 * included in any copy. This software comes with no warranty of any
 * kind. In no event will BRICS be liable for any damages resulting from
 * use of this software.
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
