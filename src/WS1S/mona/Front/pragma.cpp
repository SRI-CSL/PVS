//
// pragma.cpp
//
// MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
// 
// Reproduction of all or part of this software is permitted for
// educational or research use on condition that this copyright notice is
// included in any copy. This software comes with no warranty of any
// kind. In no event will BRICS be liable for any damages resulting from
// use of this software.

#include "pragma.h"
#include "symboltable.h"

extern SymbolTable symbolTable;

/////////// Pragma ////////////////////////////////////////////////////////////

Pragma::Pragma(String& l, PragmaKind k, int w, int i1, int i2)
  : lineInfo(l)
{
  kind   = k;
  weight = w;
  id1    = i1;
  id2    = i2;
}


