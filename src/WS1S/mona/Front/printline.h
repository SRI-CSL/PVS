//
// printline.h  
//
// MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
// 
// Reproduction of all or part of this software is permitted for
// educational or research use on condition that this copyright notice is
// included in any copy. This software comes with no warranty of any
// kind. In no event will BRICS be liable for any damages resulting from
// use of this software.

#ifndef __PRINTLINE_H
#define __PRINTLINE_H

#include "deque.h"

class FileSource: public DequeGCA<char *> {};

class Pos {
public:
  Pos() {line = col = -1; fileName = NULL;}
  Pos(int l, int c, char *f) :
    line(l), col(c), fileName(f) {}

  void printsource();

  int line, col;
  char *fileName; 
};

extern Pos dummyPos;

#endif
