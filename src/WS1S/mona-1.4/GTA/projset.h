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
