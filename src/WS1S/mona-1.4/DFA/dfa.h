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

/* See the 'MONA User Manual' for documentation */

#ifndef __DFA_H
#define __DFA_H

#include "bdd.h"

enum astat {PRODUCT, PROJECT, MINIMIZATION};

typedef enum {
  dfaIMPL = 11,
  dfaBIIMPL = 9,
  dfaAND = 8,
  dfaOR = 14
} dfaProductType;

typedef struct { 
  bdd_manager *bddm; /* manager of BDD nodes */
  int ns;            /* number of states */
  bdd_ptr *q;        /* transition array */
  int s;             /* start state */
  int *f;            /* state statuses; -1:reject, 0:don't care, +1:accept */
} DFA;

extern int dfa_in_mem; /* number of automata currently in memory */

/* dfa.c */
DFA *dfaMake(int n);
DFA *dfaMakeNoBddm(int n);
void dfaFree(DFA *a); 
void dfaNegation(DFA *a);  
void dfaRestrict(DFA *a);  
void dfaUnrestrict(DFA *a);  
DFA *dfaCopy(DFA *a);
void dfaReplaceIndices(DFA *a, int map[]);

/* product.c */
DFA *dfaProduct(DFA *a1, DFA *a2, dfaProductType mode); 

/* project.c */
DFA *dfaProject(DFA *a, unsigned index); 

/* minimize.c */
DFA *dfaMinimize(DFA *a); 

/* quotient.c */
void dfaRightQuotient(DFA *a, unsigned index); 

/* prefix.c */
void dfaPrefixClose(DFA *a);

/* analyze.c */
char *dfaMakeExample(DFA *a, int kind, int num, unsigned indices[]);
void dfaAnalyze(DFA *a, int num, char *names[], 
		unsigned indices[], char orders[], int treestyle);

/* makebasic.c */
void dfaSetup(int s, int len, int indices[]); 
void dfaAllocExceptions(int n);
void dfaStoreException(int s, char *path);
void dfaStoreState(int s);
DFA *dfaBuild(char statuses[]);

/* printdfa.c */
void dfaPrintVitals(DFA *a);
void dfaPrint(DFA *a, int num, char *names[], unsigned indices[]);
void dfaPrintGraphviz(DFA *a, int num, unsigned indices[]);
void dfaPrintVerbose(DFA *a);

/* external.c */
int dfaExport(DFA *a, char *filename, int num, char *names[], char orders[]);
DFA* dfaImport(char *filename, char ***names, int **orders);

/* basic.c */
DFA *dfaTrue();
DFA *dfaFalse();
DFA *dfaSingleton(int i);
DFA *dfaEq2(int i, int j);
DFA *dfaSubset(int i, int j);
DFA *dfaEmpty(int i);
DFA *dfaPlus2(int i, int j);
DFA *dfaMinus2(int i, int j);
DFA *dfaUnion(int i, int j, int k);
DFA *dfaInter(int i, int j, int k);
DFA *dfaSetminus(int i, int j, int k);
DFA *dfaBoolvar(int b);
DFA *dfaIn(int i, int j);
DFA *dfaEq1(int i, int j);
DFA *dfaLastPos(int i);
DFA *dfaAllPos(int i);
DFA *dfaFirstOrder(int i);
DFA *dfaLess(int i, int j);
DFA *dfaLesseq(int i, int j);
DFA *dfaConst(int n, int i);
DFA *dfaPlus1(int i, int j, int n);
DFA *dfaMinus1(int i, int j);
DFA *dfaMax(int i, int j);
DFA *dfaMin(int i, int j);
DFA *dfaPlusModulo1(int i, int j, int k);
DFA *dfaMinusModulo1(int i, int j, int k);
DFA *dfaPresbConst(int i, int n);

#endif
