/* types.h
     COPYRIGHT
          Both this software and its documentation are

              Copyright 1993, IRISA /Universite de Rennes I - France
              Copyright 1996, Doran Wilde and Vincent Loechner
              All rights reserved.

          Permission is granted to copy, use, and distribute
          for any commercial or noncommercial purpose under the terms
          of the GNU General Public license, version 2, June 1991
          (see file : LICENSING).
*/
#ifndef _types_h_
#define _types_h_

#include <stdio.h>

/* put a one in the MSB of an int (portable) */
#define MSB ((unsigned)1<<(sizeof(int)*8-1))

/* largest representable positive number */
#define TOP ((int)(MSB-1))

#define NEXT(j, b) { if (!((b)>>=1)) { (b)=MSB; (j)++; } }

/* status of last Polyhedron operation */
extern int Pol_status;

typedef struct vector {
  unsigned Size;
  int *p;
} Vector;

typedef struct matrix {
  unsigned NbRows, NbColumns;
  int **p;
  int *p_Init;
} Matrix;

typedef struct polyhedron
{ struct polyhedron *next;
  unsigned Dimension, NbConstraints, NbRays, NbEq, NbBid;
  int **Constraint;
  int **Ray;
  int *p_Init;
} Polyhedron;

/* #define emptyQ(P) (P->NbEq==(P->Dimension+1)) */
#define emptyQ(P) (P->NbRays==0)
#define universeQ(P) (P->Dimension==P->NbLines)

#endif
