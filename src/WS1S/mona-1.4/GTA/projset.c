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

#include <stdlib.h>
#include "../Mem/mem.h"
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
    set->e = (State *) mem_resize(set->e, sizeof(State)*set->allocated);
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
  mem_free(set->present);
  mem_free(set->e);
}
