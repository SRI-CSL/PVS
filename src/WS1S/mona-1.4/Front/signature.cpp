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

#include <stdlib.h>
#include <stdio.h>
#include "signature.h"
#include "offsets.h"

extern Offsets offsets;

static int sortcmp(const void *i1, const void *i2)
{
  if (*((int *) i1) < *((int *) i2))
    return -1;
  else if (*((int *) i1) > *((int *) i2))
    return 1;
  else
    return 0;
}

Signature::Signature()
{
  size = 0;
  sign = NULL;
  hashvalue = 0;
}

Signature::Signature(IdentList &idents)
{
  make(idents);
}

void
Signature::make(IdentList &idents)
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
