//
// signature.h
//
// MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
// 
// Reproduction of all or part of this software is permitted for
// educational or research use on condition that this copyright notice is
// included in any copy. This software comes with no warranty of any
// kind. In no event will BRICS be liable for any damages resulting from
// use of this software.

#ifndef __SIGNATURE_H
#define __SIGNATURE_H

#include "ident.h"

class Signature {
public:
  Signature(IdentList &idents);
  ~Signature();

  void dump(char *to);
  int operator==(const Signature &);

  unsigned  size;
  int      *sign;
  int       hashvalue;
};

#endif
