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

#include <stdlib.h>
#include <stdio.h>
#include "bdd_external.h"
#include "../Mem/mem.h"

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
    t->elms = (BddNode *) mem_resize(t->elms, sizeof(BddNode)*t->allocated); 
  }
  t->elms[t->noelems++] = *elm;
}

void tableFree(Table *t)
{
  mem_free(t->elms);
  mem_free(t);
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
      e->lo = mona_bdd_else(bddm,p);
      e->hi = mona_bdd_then(bddm,p);
      tableInsert(table, e);
      bdd_set_mark(bddm,p,table->noelems); /* table index+1 put in mark */
      export(bddm, mona_bdd_then(bddm,p), table);
      export(bddm, mona_bdd_else(bddm,p), table);
    }
    mem_free(e);
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
