/* projset.c */

/*
 * MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
 *
 * Reproduction of all or part of this software is permitted for
 * educational or research use on condition that this copyright notice is
 * included in any copy. This software comes with no warranty of any
 * kind. In no event will BRICS be liable for any damages resulting from
 * use of this software.
 */

#include <stdlib.h>
#include "projset.h"

void setInit(StateSet *set, unsigned maxSize)
{
  int i;

  set->used = set->allocated = 0;
  set->present = (char *) mem_alloc(sizeof(char)*maxSize);
  set->e = 0;

  for (i = 0; i < maxSize; i++)
    set->present[i] = 0;
}

void setInsert(StateSet *set, State s)
{
  if (set->used == set->allocated) {
    set->allocated = set->allocated*2+1;
    set->e = (State *) mem_realloc(set->e, sizeof(State)*set->allocated);
  }
  set->e[set->used] = s;
  set->present[s] = 1;
  set->used++;
}

int setEmpty(StateSet *set)
{
  return set->used == 0;
}

State setRemoveOne(StateSet *set)
{
  set->used--;
  return set->e[set->used];
}

State setRead(StateSet *set, unsigned pos)
{
  return set->e[pos];
}

unsigned setSize(StateSet *set)
{
  return set->used;
}

int setExists(StateSet *set, State s)
{
  return set->present[s] == 1;
}

void setFree(StateSet *set)
{
  free(set->present);
  free(set->e);
}
