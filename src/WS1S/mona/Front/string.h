//
// string.h
//
// MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
// 
// Reproduction of all or part of this software is permitted for
// educational or research use on condition that this copyright notice is
// included in any copy. This software comes with no warranty of any
// kind. In no event will BRICS be liable for any damages resulting from
// use of this software.

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
