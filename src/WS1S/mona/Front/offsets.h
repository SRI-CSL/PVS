//
// offsets.h
//
// MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
// 
// Reproduction of all or part of this software is permitted for
// educational or research use on condition that this copyright notice is
// included in any copy. This software comes with no warranty of any
// kind. In no event will BRICS be liable for any damages resulting from
// use of this software.

#ifndef __OFFSETS_H
#define __OFFSETS_H

#include "deque.h"

class Offsets {
public:
  void insert();
  void set();
  int off(unsigned int id);
  int maxOffset() {return max_offset;};

protected:
  Deque<int> offsetMap;
  int max_offset;
};

#endif


