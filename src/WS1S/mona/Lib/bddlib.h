/* bddlib.h - BDD definitions for external automaton library */
 
/*
 * MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
 *
 * Reproduction of all or part of this software is permitted for
 * educational or research use on condition that this copyright notice is
 * included in any copy. This software comes with no warranty of any
 * kind. In no event will BRICS be liable for any damages resulting from
 * use of this software.
 */

#ifndef __BDDLIB_H
#define __BDDLIB_H

typedef unsigned mNode; /* index into BDD array */
typedef char *mA; /* alphabet element, 0/1 array */

typedef struct mBdd {
  int idx;  /* variable index, -1 iff leaf */
  mNode lo; /* low pointer, if idx==-1 then lo contains state number */
  mNode hi; /* high pointer */
} mBdd;

#endif
