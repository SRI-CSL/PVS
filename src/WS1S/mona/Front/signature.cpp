//
// signature.cpp
//
// MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
// 
// Reproduction of all or part of this software is permitted for
// educational or research use on condition that this copyright notice is
// included in any copy. This software comes with no warranty of any
// kind. In no event will BRICS be liable for any damages resulting from
// use of this software.

#include <stdlib.h>
#include <stdio.h>
#include "signature.h"
#include "offsets.h"

extern Offsets offsets;

int sortcmp(const int *i1, const int *i2)
{
  if (*i1 < *i2)
    return -1;
  else if (*i1 > *i2)
    return 1;
  else
    return 0;
}

Signature::Signature(IdentList &idents)
{
  size = idents.size();
  int *tab1 = new int[size];
  int *tab2 = new int[size];
  IdentList::iterator i;
  unsigned int x,y,s;
  
  for (i = idents.begin(), x = 0; 
       i != idents.end(); i++, x++)
    tab1[x] = tab2[x] = offsets.off(*i);
  
  qsort((int *) tab2, size, sizeof(int), sortcmp);
  
  sign = new int[size];
  hashvalue = 0;

  for (x = 0; x < size; x++) {
    for (y = 0, s = 0; tab2[y] != tab1[x]; y++)
      if (y < size && tab2[y] != tab2[y+1])
	s++;
    sign[x] = s;
    hashvalue = hashvalue*x+sign[x];
  }

  delete[] tab1;
  delete[] tab2;
}

Signature::~Signature()
{
  delete[] sign;
}

void
Signature::dump(char *to)
{
  unsigned i;
  for (i = 0; i < size; i++)
    to += sprintf(to, "_%i", sign[i]);
}

int
Signature::operator==(const Signature &s) 
{
  if (s.hashvalue != hashvalue || s.size != size)
    return false;

  unsigned x;
  for (x = 0; x < size; x++)
    if (s.sign[x] != sign[x])
      return false;
        
  return true;
}
