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

#ifndef __OFFSETS_H
#define __OFFSETS_H

#include <assert.h>
#include "deque.h"

class Offsets {
public:
  void insert();
  void reorder();
  int off(unsigned int id) {assert(id<=max_offset); return offsetMap.get(id);}
  int maxOffset() {return max_offset;};

protected:
  Deque<int> offsetMap;
  unsigned max_offset;
};

#endif


