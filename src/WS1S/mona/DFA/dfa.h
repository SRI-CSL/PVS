/* dfa.h - DFA package interface */

/*
 * MONA is Copyright (C) 1997 BRICS. All rights reserved. 
 *
 * Reproduction of all or part of this software is permitted for
 * educational or research use on condition that this copyright notice is
 * included in any copy. This software comes with no warranty of any
 * kind. In no event will BRICS be liable for any damages resulting from
 * use of this software.
 */

/* See the 'MONA Version 1.3 User Manual' for documentation */

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
void dfaAnalyze(DFA *a_impl, DFA *a_conj, int num, char *names[], 
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
int dfaExport(DFA *a, char *filename, char *names[], int orders[]);
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
DFA *dfaLast(int i);
DFA *dfaFirstOrder(int i);
DFA *dfaLess(int i, int j);
DFA *dfaLesseq(int i, int j);
DFA *dfaConst(int n, int i);
DFA *dfaPlus1(int i, int j, int n);
DFA *dfaMinus1(int i, int j, int n);
DFA *dfaMax(int i, int j);
DFA *dfaMin(int i, int j);
DFA *dfaPlusModulo1(int i, int j, int k);
DFA *dfaMinusModulo1(int i, int j, int k);
DFA *dfaPresbConst(int i, int n);

#endif
