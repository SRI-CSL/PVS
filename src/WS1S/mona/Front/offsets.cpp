//
// offsets.cpp
//
// MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
// 
// Reproduction of all or part of this software is permitted for
// educational or research use on condition that this copyright notice is
// included in any copy. This software comes with no warranty of any
// kind. In no event will BRICS be liable for any damages resulting from
// use of this software.

#include "offsets.h"

void
Offsets::insert()
{
  offsetMap.push_back(offsetMap.size());
  max_offset = offsetMap.size();
}

void
Offsets::set()
{
  ////// handle pragmas ///////

  max_offset = offsetMap.size();
}

int 
Offsets::off(unsigned id)
{
  return offsetMap.get(id) ;
}

