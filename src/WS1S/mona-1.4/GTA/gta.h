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

#ifndef __GTA_H
#define __GTA_H

#include "bdd.h"

/* Guided Tree Automaton */

typedef unsigned State; /* automaton state */
typedef unsigned SsId;  /* state space id */

typedef enum {
  gtaSSUNIVHAT, gtaSSORHAT, gtaSSORLEAF, gtaSSAND, gtaSSDUMMY
} SsKind; // state space kind

typedef struct {
  unsigned numSs;         /* number of state spaces */
  SsId *muLeft;           /* array of left successors */
  SsId *muRight;          /* array of right successors */
  SsId **hitsLeft;        /* [i]: array of s.s. that have i as left suc.*/
  unsigned *numHitsLeft;  /* [i]: size of hitsLeft[i] */
  SsId **hitsRight;       /* [i]: array of s.s. that have i as right suc. */
  unsigned *numHitsRight; /* [i]: size of hitsRight[i] */
  char **ssName;          /* [i]: name of state space i */
  int *ssUniv;            /* [i]: universe of state space i, -1:none, -2:hat */
  char *ssUnivRoot;       /* [i]: non-zero if s.s. i is root of a universe */
  int *ssType;            /* [i]: tree type of s.s. i, -1 if not type root */
  SsKind *ssKind;         /* [i]: kind of s.s. i, 0 if not using types */
  char **univName;        /* [i]: name of universe i */
  char **univPos;         /* [i]: pos ('0'/'1' string) of universe i */
  SsId **univSS;          /* [i]: state spaces for universe i */
  unsigned *numUnivSS;    /* [i]: size of univSS[i] */
  unsigned numUnivs;      /* number of universes */
} Guide;

extern Guide guide; /* common global guide is declared in the front end */

typedef struct {
  State initial;         /* initial state */
  unsigned size;         /* number of states */
  unsigned ls, rs;       /* dimensions of behaviour matrix incl. unused */
  bdd_handle *behaviour; /* behaviour[i*rs+j]: BDD ptr for state pair (i,j) */
  bdd_manager *bddm;     /* BDD manager */
} StateSpace;

typedef struct {
  int *final;     /* final-status vector, -1:reject, 0:don't care, +1:accept */
  StateSpace *ss; /* array of state spaces */
} GTA;

/* misc. */

typedef enum {
  gtaIMPL = 11, 
  gtaBIIMPL = 9, 
  gtaAND = 8, 
  gtaOR = 14
} gtaProductType;

typedef char *SSSet; /* set of state-spaces, bitvector of size guide.numSs */

/* macro for indexing into behaviour matrix */
#define BEH(ss, i, j) ss.behaviour[(i)*ss.rs+(j)]

/* tree for examples and counter-examples */
typedef struct Tree {
  SsId d; /* state space for tree node */
  SsId state; /* state reachable by tree */
  bdd_manager *bddm; /* the manager into which behavior_handle points */
  bdd_handle behavior_handle; /* BDD handle of behavior of state */
  int depth; 
  int size; 
  boolean empty; /* if tree is empty, then left, right makes no sense */
  struct Tree *left, *right; /* left and right succesors (when not "empty") */
  struct Tree *next; /* all trees allocated are linked through "next" */
} Tree;

/* recursive type */
typedef struct gtaType {
  char *name;            /* name of type */
  int numVariants;       /* number of variants */
  char **variantName;    /* [i]: name of variant i */
  char **variantPos;     /* [i]: position of variant i */
  int *numComponents;    /* [i]: number of components in variant i */
  char ***componentName; /* [i][j]: name of comp. j in var. i */
  char ***componentPos;  /* [i][j]: position of comp. j in var. i */
  int **componentType;   /* [i][j]: type number of comp. j in var. i */
  char ***ct;            /* type names - only used during setup */
} gtaType;

extern gtaType *treetypes; /* array of tree types (0 if no types) */
extern int num_types;      /* number of types */
extern int gta_in_mem; /* number of automata currently in memory */

/* functions */

/* gta.c */
void makeGuide(unsigned numSs, SsId muLeft[], SsId muRight[], char *ssName[],
	       unsigned numUnivs, char *univPos[], char *univName[],
	       int ssType[], SsKind *ssKind);
void makeDefaultGuide(unsigned numUnivs, char *univName[]);
void freeGuide();
void printGuide();
int hasMember(SSSet ss, SsId s);
int checkAllCovered(); /* check guide covered by universes */
int checkDisjoint(); /* check each s.s. associated to max one universe */
int checkAllUsed(); /* check all state spaces used */
GTA *gtaMake();
void gtaFree(GTA* a);

/* external.c */
int gtaExport(GTA *a, char *filename, int num, char *names[], 
	      char orders[], SSSet *statespaces, int opt_inhacc);
GTA *gtaImport(char *filename, char ***names, int **orders, SSSet **ss,
	       int set_guideandtypes); /* set or check guide and types */

/* printgta.c */
void gtaPrintVerbose(GTA *a);
void gtaPrint(GTA *a, unsigned indices[], unsigned num, char *names[],
	      int inherited_acceptance); /* analyze inh. acc. if true */
void gtaPrintVitals(GTA *a);
void gtaPrintTotalSize(GTA *a);

/* analyze.c */
void gtaAnalyze(GTA *a, unsigned num, char *names[], 
		unsigned indices[], int opt_gs, int opt_gc);
Tree *gtaMakeExample(GTA *a, int kind);
void gtaFreeTrees();

/* types.c */
void initTreetypes(int num);
int initTreetype(char *name, int numVariants);
void initTreetypeVariant(char *name, int numComponents);
void setTreetypeComponent(char *name, char *type);
void setVariantPos(int type, int var, char *pos);
void setComponentPos(int type, int var, int comp, char *pos);
void setComponentTypes();
void freeTreetypes();
void gtaTypeAnalyze(GTA *a, unsigned num, char *names[], char orders[],
		    unsigned indices[], int *univs[], int trees[]);

/* replace_indices.c */
void gtaReplaceIndices(GTA *a, unsigned map[]);

/* copy.c */
GTA *gtaCopy(GTA *a); 

/* negation.c */
void gtaNegation(GTA *a);

/* product.c */
GTA *gtaProduct(GTA *a1, GTA *a2, gtaProductType mode);

/* project.c */
GTA *gtaQuotientAndProject(GTA *a, unsigned index, int quotient);

/* minimize.c */
GTA *gtaMinimize(GTA *a);

/* reachable.c */
GTA *gtaReachable(GTA *a);

/* restrict.c */
void gtaRestrict(GTA *a);
void gtaUnrestrict(GTA *a);

/* analyze_acceptance.c */
boolean ***gtaCalcInheritedAcceptance(GTA *a); 
void gtaFreeInheritedAcceptance(boolean ***ia);

/* makebasic.c */
void gtaSetup(unsigned rootsize);
void gtaSetupDelta(SsId d, unsigned lsize, unsigned rsize, 
		   int indices[], unsigned num);
void gtaAllocExceptions(SsId l, SsId r, unsigned n);
void gtaStoreException(unsigned s, char *path);
void gtaStoreDefault(unsigned s);
void gtaBuildDelta(State initial);
GTA *gtaBuild(char statuses[]);

/* basic.c */
GTA *gtaEq1(int i, int j, SSSet s_i, SSSet s_j);
GTA *gtaEq2(int i, int j, SSSet s_i, SSSet s_j);
GTA *gtaTrue();
GTA *gtaFalse();
GTA *gtaSingleton(int i, SSSet s_i);
GTA *gtaIn(int i, int j, SSSet s_i, SSSet s_j);
GTA *gtaFirstOrder(int i, SSSet s_i);
GTA *gtaAllPos(int i, SSSet s_i);
GTA *gtaDot1(int i, int j, SSSet s_i, SSSet s_j);
GTA *gtaDot0(int i, int j, SSSet s_i, SSSet s_j);
GTA *gtaUp(int i, int j, SSSet s_i, SSSet s_j);
GTA *gtaEmpty(int i, SSSet s_i);
GTA *gtaSub(int i, int j, SSSet s_i, SSSet s_j);
GTA *gtaRoot(int i, SSSet s_i, SSSet s_);
GTA *gtaLess(int i, int j, SSSet s_i, SSSet s_j);
GTA *gtaLesseq(int i, int j, SSSet s_i, SSSet s_j);
GTA *gtaBoolvar(int alpha);
GTA *gtaBoolvar2(int alpha);
GTA *gtaUnion(int i, int j, int k, SSSet s_ijk);
GTA *gtaInter(int i, int j, int k, SSSet s_ijk);
GTA *gtaSetminus(int i, int j, int k, SSSet s_ijk);
GTA *gtaInStateSpace(int i, SSSet s_s, SSSet s_i);
GTA *gtaWellFormedTree(int i, SSSet s_i);
GTA *gtaSomeType(int i, SSSet s_i);

#endif
