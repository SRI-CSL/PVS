/* gtalib.h - library for using MONA-generated external GTAs */
 
/*
 * MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
 *
 * Reproduction of all or part of this software is permitted for
 * educational or research use on condition that this copyright notice is
 * included in any copy. This software comes with no warranty of any
 * kind. In no event will BRICS be liable for any damages resulting from
 * use of this software.
 */

#ifndef __GTALIB_H
#define __GTALIB_H

#include "bddlib.h"

typedef unsigned mgState; /* automaton state */
typedef unsigned mgId;    /* state space id */

typedef enum mgKind {mgREJECT = -1, mgDONTCARE = 0, mgACCEPT = 1} mgKind;

typedef struct mgStateSpace {
  mgState initial;      /* initial state */
  unsigned numStates;   /* number of states */
  unsigned numBddNodes; /* number of BDD nodes */
  mBdd *bddNode;        /* array of BDD nodes */
  mNode **behaviour;    /* matrix of BDD nodes, one pointer per pair of states */
  mgId leftSS;          /* id of left state space */
  mgId rightSS;         /* id of right state space */
  char *name;           /* state space name */
} mgStateSpace;

typedef struct mgUniverse {
  char *name; /* universe name */
  char *pos;  /* position ('0'/'1' string) */
} mgUniverse;

typedef struct mgVariable {
  char *name;        /* variable name */
  int order;         /* order (0/1/2) */
  unsigned numVarSS; /* number of variable state-spaces */
  mgId *varSS;       /* array of variable state-spaces */
} mgVariable;

typedef struct mgGta {
  unsigned numSS;           /* number of state spaces */
  mgStateSpace *stateSpace; /* array of state spaces */
  unsigned numUnivs;        /* number of universes */
  mgUniverse *universe;     /* array of universes */
  unsigned numVars;         /* number of free variables */
  mgVariable *var;          /* array of variables */
  mgKind *final;            /* initial state space state kinds */
} mgGta;

typedef struct mgTreeNode {
  mA a;                            /* alphabet element */
  struct mgTreeNode *left, *right; /* successors */
  mgId id;                         /* state space id */
  mgState state;                   /* automaton state */
} mgTreeNode;

/* load GTA from text file */
mgGta *mgLoad(char *filename); /* returns 0 if unable to load automaton */

/* write GTA to text file 
   notice: when using external automata, the user has 
   to ensure that the files are consistent and that 
   involved BDDs are properly reduced and ordered */
int mgStore(mgGta *gta, char *filename); /* returns 0 if unable to write file */

/* clean up GTA */
void mgFree(mgGta *gta);

/* transition function */
mgState mgDelta(mgGta *gta, mgId ss, mgState left, mgState right, mA a);

/* assign state space ids and automaton states to tree nodes */
void mgAssign(mgGta *gta, mgTreeNode *t, mgId id);

/* check acceptance of input tree */
int mgCheck(mgGta *gta, mgTreeNode *t);

#endif
