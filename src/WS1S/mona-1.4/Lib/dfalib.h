/* dfalib.h - library for using MONA-generated external DFAs */
 
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
