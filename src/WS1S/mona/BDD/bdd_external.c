/* bdd_external.c */

/*
 * MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
 *
 * Reproduction of all or part of this software is permitted for
 * educational or research use on condition that this copyright notice is
 * included in any copy. This software comes with no warranty of any
 * kind. In no event will BRICS be liable for any damages resulting from
 * use of this software.
 */

#include <stdlib.h>
#include "bdd_external.h"
#include <stdio.h>

Table *tableInit() 
{
  Table *t = (Table *) mem_alloc(sizeof(Table));
  t->allocated = 0;
  t->noelems = 0;
  t->elms = 0;
  return t;
}

void tableInsert(Table *t, BddNode *elm)
{
  if (t->allocated == t->noelems) {
    t->allocated = t->allocated * 2 + 5;
    t->elms = (BddNode *) mem_realloc(t->elms, sizeof(BddNode)*t->allocated); 
  }
  t->elms[t->noelems++] = *elm;
}

void tableFree(Table *t)
{
  free(t->elms);
  free(t);
}

/* EXPORT */

void export(bdd_manager *bddm, unsigned p, Table *table) 
{
  if (!bdd_mark(bddm,p)) {
    BddNode *e = (BddNode *) mem_alloc(sizeof(BddNode));
    if (bdd_is_leaf(bddm,p)) {
      e->idx = -1;
      e->lo = bdd_leaf_value(bddm, p);
      e->hi = 0;
      tableInsert(table, e);
      bdd_set_mark(bddm,p,table->noelems); /* table index+1 put in mark */
    }
    else {
      e->idx = bdd_ifindex(bddm,p);
      e->lo = ws1s___bdd_else(bddm,p);
      e->hi = ws1s___bdd_then(bddm,p);
      tableInsert(table, e);
      bdd_set_mark(bddm,p,table->noelems); /* table index+1 put in mark */
      export(bddm, ws1s___bdd_then(bddm,p), table);
      export(bddm, ws1s___bdd_else(bddm,p), table);
    }
    free(e);
  }
}

/* IMPORT */

BddNode *table;
bdd_manager *import_bddm;

unsigned make_node(int i)
{
  if (table[i].p == -1) { /* bdd_node not created yet */
    if (table[i].idx == -1)  /* a leaf */
      table[i].p = bdd_find_leaf_sequential(import_bddm,
                                            table[i].lo);
    else {
      invariant(table[i].lo != table[i].hi);
      table[i].lo = make_node(table[i].lo);
      table[i].hi = make_node(table[i].hi);
      table[i].p = 
        bdd_find_node_sequential(import_bddm, 
                                 table[i].lo,
                                 table[i].hi,
                                 table[i].idx);
    }
  }
  /*  printf("make_note i: %4d  table[i].lo: %4d table[i].hi: %4d "  
      "table[i].idx %4d p: %4d\n", 
      i,table[i].lo, table[i].hi, table[i].idx, table[i].p);
      */
  return table[i].p;
}
