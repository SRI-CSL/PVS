/* gtalib.h - library for using MONA-generated external GTAs */
 
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

#ifndef __GTALIB_H
#define __GTALIB_H

#include "bddlib.h"

typedef unsigned mgState; /* automaton state */
typedef unsigned mgId;    /* state space id */

typedef enum mgKind {mgREJECT = -1, mgDONTCARE = 0, mgACCEPT = 1} mgKind;

typedef struct mgInhAcc { /* inherited acceptance information */
  char canAccept;   /* can lead to accept */
  char canDontCare; /* can lead to don't care */
  char canReject;   /* can lead to reject */
} mgInhAcc;

typedef struct mgStateSpace { /* GTA state space */
  mgState initial;      /* initial state */
  unsigned numStates;   /* number of states */
  unsigned numBddNodes; /* number of BDD nodes */
  mBdd *bddNode;        /* array of BDD nodes */
  mNode **behaviour;    /* matrix of BDD nodes, one pointer per pair of states */
  mgId leftSS;          /* id of left state space */
  mgId rightSS;         /* id of right state space */
  char *name;           /* state space name */
  mgInhAcc *inhacc;     /* inherited acceptance (NULL if not available) */
} mgStateSpace;

typedef struct mgTypeComponent { /* component of variant of recursive type */
  char *name;    /* component name */
  char *pos;     /* position bitstring */
  unsigned type; /* component type number */
} mgTypeComponent;

typedef struct mgTypeVariant { /* variant of recursive type */
  char *name;                 /* variant name */
  char *pos;                  /* position bitstring */
  unsigned numComponents;     /* number of components */
  mgTypeComponent *component; /* variant components */
} mgTypeVariant;

typedef struct mgType { /* recursive type */
  char *name;             /* type name */
  unsigned numVariants;   /* number of variants */
  mgTypeVariant *variant; /* type variants */
} mgType;

typedef struct mgUniverse { /* GTA universe information */
  char *name; /* universe name */
  char *pos;  /* position ('0'/'1' string) */
  unsigned type; /* universe root type (N/A if no types) */
} mgUniverse;

typedef struct mgVariable { /* variable information */
  char *name;        /* variable name */
  int order;         /* order (0/1/2) */
  unsigned numVarSS; /* number of variable state-spaces */
  mgId *varSS;       /* array of variable state-spaces */
} mgVariable;

typedef struct mgGta { /* Guided Tree Automaton */
  unsigned numSS;           /* number of state spaces */
  mgStateSpace *stateSpace; /* array of state spaces */
  unsigned numUnivs;        /* number of universes */
  mgUniverse *universe;     /* array of universes */
  unsigned numVars;         /* number of free variables */
  mgVariable *var;          /* array of variables */
  mgKind *final;            /* initial state space state kinds */
  unsigned numTypes;        /* number of types (0: not using types) */
  mgType *type;             /* array of types (NULL if not using types) */
} mgGta;

typedef struct mgTreeNode { /* labelled tree */
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
