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
