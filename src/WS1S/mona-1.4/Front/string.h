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

#ifndef __STRING_H
#define __STRING_H

#include <iostream.h>
#include <string.h>
#include <stdio.h>

class String {
  struct srep {
    char *s;
    int n;
    srep() {n=1;}
  };

  srep *p;

public:

  String(const char *s) 
  {
    p = new srep;
    p->s = new char[strlen(s)+1];
    strcpy(p->s, s);
  }

  String(const String &s) 
  {
    s.p->n++;
    p = s.p;
  }

  String(const int i) 
  {
    p = new srep;
    p->s = new char[12];
    sprintf(p->s, "%d", i);
  }

  friend String &operator+(const String &s1, const String &s2)
  {
    char t[1000];
    strcpy(t, s1.p->s);
    strcat(t, s2.p->s);
    return *(new String(t));
  }

  friend ostream &operator<<(ostream &o, const String &s) 
  {
    return o << s.p->s;
  }

  ~String() 
  {
    if (--p->n == 0) {
      delete[] p->s;
      delete p;
    }
  }
};

#endif
