/* projset.h */

/*
 * MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
 *
 * Reproduction of all or part of this software is permitted for
 * educational or research use on condition that this copyright notice is
 * included in any copy. This software comes with no warranty of any
 * kind. In no event will BRICS be liable for any damages resulting from
 * use of this software.
 */

#ifndef __PROJSET_H
#define __PROJSET_H

#include "gta.h"

typedef struct {
  unsigned used, allocated;
  char *present;
  State *e;
} StateSet;

void setInit(StateSet *set, unsigned maxSize);
void setInsert(StateSet *set, State s);
int setEmpty(StateSet *set);
State setRemoveOne(StateSet *set);
State setRead(StateSet *set, unsigned pos);
unsigned setSize(StateSet *set);
int setExists(StateSet *set, State s);
void setFree(StateSet *set);

#endif
