/* dfalib.h - library for using MONA-generated external DFAs */
 
/*
 * MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
 *
 * Reproduction of all or part of this software is permitted for
 * educational or research use on condition that this copyright notice is
 * included in any copy. This software comes with no warranty of any
 * kind. In no event will BRICS be liable for any damages resulting from
 * use of this software.
 */

#ifndef __DFALIB_H
#define __DFALIB_H

#include <stdio.h>
#include "bddlib.h"

typedef unsigned mdState; /* index into state array */
typedef enum mdKind {mdREJECT = -1, mdDONTCARE = 0, mdACCEPT = 1} mdKind;

typedef struct mdDfa {
  unsigned states;    /* number of automaton states */
  mdState q0;         /* initial state */
  mdKind *f;          /* state kinds, reject/don't-care/accept */
  unsigned numVars;   /* number of free variables */
  char **var;         /* array of names of free variables */
  int *order;         /* array of orders of free variables (0/1/2) */
  mdState **incident; /* -1 terminated arrays of states incident to q_i */
  unsigned bddNodes;  /* number of BDD nodes */
  mBdd *bdd;          /* array of BDD nodes */
  mNode *behaviour;   /* array of pointers to BDD nodes, one for each state */
} mdDfa; 

/* load DFA from text file */
mdDfa *mdLoad(char *filename); /* returns 0 if unable to load automaton */

/* write DFA to text file 
   notice: when using external automata, the user has 
   to ensure that the files are consistent and that 
   involved BDDs are properly reduced and ordered */
int mdStore(mdDfa *dfa, char *filename); /* returns 0 if unable to write */ 

/* clean up DFA */
void mdFree(mdDfa *dfa);

/* transition function */
mdState mdDelta(mdDfa *dfa, mdState s, mA a); 

#endif
