//
// pragma.h
//
// MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
// 
// Reproduction of all or part of this software is permitted for
// educational or research use on condition that this copyright notice is
// included in any copy. This software comes with no warranty of any
// kind. In no event will BRICS be liable for any damages resulting from
// use of this software.

#ifndef __PRAGMA_H
#define __PRAGMA_H

#include "string.h"
#include "deque.h"

enum PragmaKind {
  pLess,
  pGreater,
  pEqueal
};

class Pragma {
public:
  Pragma(String& l, PragmaKind k, int w, int i1, int i2);

protected:
  String     lineInfo;
  PragmaKind kind;
  int        weight;
  int        id1;
  int        id2;
};

class PragmaList: public Deque<Pragma *> {};

#endif
